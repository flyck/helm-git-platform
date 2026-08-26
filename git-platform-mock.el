;;; git-platform-mock.el --- SQLite-backed demo backend for git-platform -*- lexical-binding: t; -*-

;;; Commentary:

;; A fake git-platform backend for demos, screencasts and offline play.
;; Pull requests, comments and reviewer states live in a local SQLite
;; database (Emacs 29+ built-in sqlite), so every interaction in the UI
;; -- replying to a comment, resolving it, approving a PR, toggling
;; draft -- actually persists and shows up on the next refresh, without
;; any network or credentials.
;;
;; Usage:
;;
;;   (require 'git-platform-mock)
;;   M-x git-platform-mock-enable      ;; switch the whole UI to the mock
;;   M-x gp-helm                       ;; browse the seeded demo PRs
;;   ...
;;   M-x git-platform-mock-disable     ;; back to the real backend
;;
;; For repeatable takes, `git-platform-mock-reset' (or a prefix argument
;; to `git-platform-mock-enable') deletes the database and reseeds it,
;; and restarts the simulated-pipeline clock.
;;
;; What the seed contains (workspace "acme", you = `git-platform-mock-user-name'):
;;
;;   #101 webshop  · yours, reviewers Alice ✅ / Bob ⏳, comment threads,
;;                   an unresolved nit to resolve on camera, and a LIVE
;;                   pipeline: Build finishes ~35s after enable, tests
;;                   run with a ticking duration, all green after ~90s.
;;   #102 webshop  · your draft (ready/draft toggle demo).
;;   #103 billing  · Alice's PR awaiting YOUR review; has an ⊘ outdated
;;                   inline comment and a FAILED pipeline with a log.
;;   #104 infra    · Bob's PR awaiting your review; pipeline paused at an
;;                   open manual gate (⏸) -- running the step "applies to
;;                   production" live over ~20s.
;;   #105 billing  · merged (visible with C-c m).
;;
;; Implementation notes: the class subclasses `git-platform-bitbucket',
;; so all JSON-shape accessors are inherited -- the mock simply produces
;; Bitbucket-shaped alists.  Every network operation of the protocol is
;; overridden to read/write SQLite instead.  The three `bitbucket-*'
;; async functions the UI calls directly (detail loader, helm pipeline
;; bubbles) are covered by advice that short-circuits to the database
;; while the mock backend is active.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'gp-log)
(require 'git-platform)
(require 'git-platform-bitbucket)
(require 'bitbucket-api)

(defclass git-platform-mock (git-platform-bitbucket) ()
  :documentation "SQLite-backed demo backend (no network).")

(defgroup git-platform-mock nil
  "Fake, SQLite-backed git-platform backend for demos."
  :group 'bitbucket)

(defcustom git-platform-mock-db-file
  (locate-user-emacs-file "gp-mock-demo.sqlite")
  "Where the mock backend stores its pull requests and comments."
  :type 'file :group 'git-platform-mock)

(defcustom git-platform-mock-user-name "Felix"
  "Display name of \"you\" in the demo data."
  :type 'string :group 'git-platform-mock)

(defconst gp-mock--me "{mock-felix}"
  "The demo user's account uuid.")

(defconst gp-mock--users
  '(("{mock-alice}" . "Alice Meyer")
    ("{mock-bob}"   . "Bob Tanaka")
    ("{mock-carol}" . "Carol Novak"))
  "The demo co-workers (uuid . display name).")

;;;; Database ------------------------------------------------------------------

(defvar gp-mock--db nil
  "Open handle on `git-platform-mock-db-file', or nil.")

(defun gp-mock--db ()
  "Return the open demo database, creating and seeding it on first use."
  (unless (and (fboundp 'sqlite-available-p) (sqlite-available-p))
    (error "git-platform-mock needs an Emacs built with SQLite (29+)"))
  (unless (and gp-mock--db (sqlitep gp-mock--db))
    (setq gp-mock--db (sqlite-open (expand-file-name git-platform-mock-db-file)))
    (gp-mock--init-schema gp-mock--db)
    (when (zerop (caar (sqlite-select gp-mock--db "SELECT COUNT(*) FROM prs")))
      (gp-mock--seed gp-mock--db)))
  gp-mock--db)

(defun gp-mock--init-schema (db)
  "Create the mock tables in DB when missing."
  (sqlite-execute db "
CREATE TABLE IF NOT EXISTS prs (
  id            INTEGER PRIMARY KEY,
  full_name     TEXT NOT NULL,
  title         TEXT NOT NULL,
  description   TEXT DEFAULT '',
  state         TEXT DEFAULT 'OPEN',
  draft         INTEGER DEFAULT 0,
  author_uuid   TEXT, author_name TEXT,
  source_branch TEXT, source_commit TEXT,
  dest_branch   TEXT DEFAULT 'main',
  created_on    TEXT, updated_on TEXT,
  commit_count  INTEGER DEFAULT 1,
  diff          TEXT DEFAULT ''
)")
  (sqlite-execute db "
CREATE TABLE IF NOT EXISTS comments (
  id          INTEGER PRIMARY KEY,
  pr_id       INTEGER NOT NULL,
  full_name   TEXT NOT NULL,
  parent_id   INTEGER,
  user_uuid   TEXT, user_name TEXT,
  content     TEXT,
  inline_path TEXT, inline_to INTEGER,
  resolved_by TEXT,
  deleted     INTEGER DEFAULT 0,
  created_on  TEXT, updated_on TEXT
)")
  (sqlite-execute db "
CREATE TABLE IF NOT EXISTS reactions (
  comment_id INTEGER NOT NULL,
  user_uuid  TEXT NOT NULL,
  user_name  TEXT,
  content    TEXT NOT NULL,
  PRIMARY KEY (comment_id, user_uuid, content)
)")
  (sqlite-execute db "
CREATE TABLE IF NOT EXISTS participants (
  pr_id     INTEGER NOT NULL,
  user_uuid TEXT NOT NULL,
  user_name TEXT,
  role      TEXT DEFAULT 'REVIEWER',
  state     TEXT,
  PRIMARY KEY (pr_id, user_uuid)
)"))

;;;; Row -> Bitbucket-shaped alists ---------------------------------------------

(defconst gp-mock--pr-cols
  (concat "p.id, p.full_name, p.title, p.description, p.state, p.draft,"
          " p.author_uuid, p.author_name, p.source_branch, p.source_commit,"
          " p.dest_branch, p.created_on, p.updated_on, p.commit_count,"
          " (SELECT COUNT(*) FROM comments c"
          "   WHERE c.pr_id = p.id AND c.deleted = 0)")
  "Column list every PR SELECT uses, in `gp-mock--pr-row' order.")

(defun gp-mock--participants (pr-id)
  "Return PR-ID's participants as Bitbucket-shaped alists."
  (mapcar
   (pcase-lambda (`(,uuid ,name ,role ,state))
     `((role . ,role)
       (state . ,state)
       ,@(when (equal state "approved") '((approved . t)))
       (user (uuid . ,uuid) (display_name . ,name))))
   (sqlite-select (gp-mock--db)
                  "SELECT user_uuid, user_name, role, state
                   FROM participants WHERE pr_id = ? ORDER BY user_uuid"
                  (list pr-id))))

(defun gp-mock--pr-row (row)
  "Turn a PR ROW (`gp-mock--pr-cols' order) into a Bitbucket-shaped alist."
  (pcase-let ((`(,id ,full ,title ,desc ,state ,draft ,auuid ,aname
                 ,sbranch ,scommit ,dbranch ,created ,updated ,_ncommits
                 ,ncomments)
               row))
    `((id . ,id)
      (title . ,title)
      (description . ,desc)
      (state . ,state)
      ,@(when (eql draft 1) '((draft . t)))
      (author (uuid . ,auuid)
              (display_name . ,aname)
              (links (avatar (href . nil))))
      (source (branch (name . ,sbranch)) (commit (hash . ,scommit)))
      (destination (branch (name . ,dbranch))
                   (repository (full_name . ,full)
                               (slug . ,(cadr (split-string full "/")))))
      (comment_count . ,ncomments)
      (created_on . ,created)
      (updated_on . ,updated)
      (participants . ,(gp-mock--participants id))
      (links (html (href . ,(format "https://bitbucket.org/%s/pull-requests/%s"
                                    full id)))))))

(defun gp-mock--select-prs (where &optional args)
  "Return PR alists matching the SQL WHERE clause (with ARGS)."
  (mapcar #'gp-mock--pr-row
          (sqlite-select (gp-mock--db)
                         (format "SELECT %s FROM prs p WHERE %s
                                  ORDER BY p.updated_on DESC"
                                 gp-mock--pr-cols where)
                         args)))

(defun gp-mock--comment-row (row)
  "Turn a comment ROW into a Bitbucket-shaped alist."
  (pcase-let ((`(,id ,pr-id ,full ,parent ,uuid ,name ,content
                 ,ipath ,ito ,resolved-by ,created ,updated)
               row))
    `((id . ,id)
      (content (raw . ,content))
      (user (uuid . ,uuid)
            (display_name . ,name)
            (links (avatar (href . nil))))
      (created_on . ,created)
      ,@(when updated `((updated_on . ,updated)))
      ,@(when parent `((parent (id . ,parent))))
      ,@(when ipath `((inline (path . ,ipath) (to . ,ito))))
      ,@(when resolved-by `((resolution (user (display_name . ,resolved-by)))))
      (links (html (href . ,(format "https://bitbucket.org/%s/pull-requests/%s#comment-%s"
                                    full pr-id id)))))))

(defconst gp-mock--comment-cols
  "id, pr_id, full_name, parent_id, user_uuid, user_name, content,
   inline_path, inline_to, resolved_by, created_on, updated_on")

(defun gp-mock--comment (comment-id)
  "Return the comment alist for COMMENT-ID."
  (when-let* ((row (car (sqlite-select
                         (gp-mock--db)
                         (format "SELECT %s FROM comments WHERE id = ?"
                                 gp-mock--comment-cols)
                         (list comment-id)))))
    (gp-mock--comment-row row)))

(defun gp-mock--state-clause (states)
  "Return \"AND state IN (...)\" for STATES (a list), or \"\" for nil."
  (if (null states) ""
    (format " AND p.state IN (%s)"
            (mapconcat (lambda (s) (format "'%s'" s)) states ","))))

(defun gp-mock--iso (&optional ago)
  "Return an ISO-8601 UTC timestamp AGO seconds in the past."
  (gp-mock--iso-at (time-subtract (current-time) (or ago 0))))

(defun gp-mock--iso-at (time)
  "Return TIME as an ISO-8601 UTC timestamp string."
  (format-time-string "%Y-%m-%dT%H:%M:%S+00:00" time t))

(defun gp-mock--touch-pr (pr-id)
  "Bump PR-ID's updated_on to now."
  (sqlite-execute (gp-mock--db)
                  "UPDATE prs SET updated_on = ? WHERE id = ?"
                  (list (gp-mock--iso) pr-id)))

;;;; Protocol: users and PR lists ----------------------------------------------

(cl-defmethod gp--user-uuid ((_ git-platform-mock))
  gp-mock--me)

(cl-defmethod gp--workspace-pull-requests ((_ git-platform-mock)
                                           &optional uuid state max-items)
  (let* ((uuid (or uuid gp-mock--me))
         (prs (gp-mock--select-prs
               (concat "(p.author_uuid = ? OR EXISTS
                          (SELECT 1 FROM participants pa
                           WHERE pa.pr_id = p.id AND pa.user_uuid = ?))"
                       (if state " AND p.state = ?" ""))
               (if state (list uuid uuid state) (list uuid uuid)))))
    (if max-items (seq-take prs max-items) prs)))

(cl-defmethod gp--workspace-pull-requests-async ((backend git-platform-mock) callback
                                                 &optional uuid state max-items)
  ;; a small delay keeps the overview's "⏳ refreshing…" visible for a beat,
  ;; like live, and exercises the async redraw path in the mock UI
  (let ((prs (gp--workspace-pull-requests backend uuid state max-items)))
    (run-at-time 0.4 nil callback t prs)))

(defun gp-mock--reviewing (uuid states)
  "Return PRs where UUID reviews someone else's work, filtered to STATES."
  (gp-mock--select-prs
   (concat "p.author_uuid != ? AND EXISTS
              (SELECT 1 FROM participants pa
               WHERE pa.pr_id = p.id AND pa.user_uuid = ?
                 AND pa.role = 'REVIEWER')"
           (gp-mock--state-clause states))
   (list uuid uuid)))

(cl-defmethod gp--reviewing-pull-requests ((_ git-platform-mock)
                                           &optional uuid limit states)
  (let ((prs (gp-mock--reviewing (or uuid gp-mock--me) (or states '("OPEN")))))
    (if limit (seq-take prs limit) prs)))

(cl-defmethod gp--reviewing-pull-requests-async ((_ git-platform-mock)
                                                 uuid states on-batch on-done
                                                 &optional limit)
  ;; a small delay keeps the "scanning…" row visible for a beat, like live
  (let ((prs (gp-mock--reviewing (or uuid gp-mock--me) states)))
    (when limit (setq prs (seq-take prs limit)))
    (run-at-time 0.4 nil on-batch prs)
    (run-at-time 0.6 nil on-done)))

(cl-defmethod gp--open-pull-requests-async ((_ git-platform-mock)
                                            states on-batch on-done
                                            &optional limit)
  (let ((prs (gp-mock--select-prs
              (concat "1=1" (gp-mock--state-clause (or states '("OPEN")))))))
    (when limit (setq prs (seq-take prs limit)))
    (run-at-time 0.4 nil on-batch prs)
    (run-at-time 0.6 nil on-done)))

(cl-defmethod gp--pull-request ((_ git-platform-mock) full-name id)
  (car (gp-mock--select-prs "p.full_name = ? AND p.id = ?"
                            (list full-name id))))

(cl-defmethod gp--open-pr-for-branch ((_ git-platform-mock) full-name branch)
  (car (gp-mock--select-prs
        "p.full_name = ? AND p.source_branch = ? AND p.state = 'OPEN'"
        (list full-name branch))))

(cl-defmethod gp--repo-pull-requests ((_ git-platform-mock) full-name
                                      &optional state)
  (gp-mock--select-prs "p.full_name = ? AND p.state = ?"
                       (list full-name (or state "OPEN"))))

(cl-defmethod gp--repo-open-pr-count ((_ git-platform-mock) full-name)
  (caar (sqlite-select (gp-mock--db)
                       "SELECT COUNT(*) FROM prs p
                        WHERE p.full_name = ? AND p.state = 'OPEN'"
                       (list full-name))))

(cl-defmethod gp--repo-default-branch ((_ git-platform-mock) _full-name)
  "main")

(cl-defmethod gp--backend-name ((_ git-platform-mock))
  "Report `bitbucket'.
The mock impersonates Bitbucket's shapes and semantics, so
backend-keyed configuration (`gp-comment-delete-others') behaves in
demos exactly as it will against the real thing."
  'bitbucket)

(defun gp-mock--reviewer-alist (users)
  "Reshape USERS ((UUID . NAME)…) into reviewer alists."
  (mapcar (pcase-lambda (`(,uuid . ,name))
            `((uuid . ,uuid) (display_name . ,name)))
          users))

(cl-defmethod gp--repo-default-reviewers ((_ git-platform-mock) _full-name)
  ;; only the first co-worker is a repo default; the rest are merely
  ;; suggestable, so the create form demonstrates both groups
  (gp-mock--reviewer-alist (seq-take gp-mock--users 1)))

(cl-defmethod gp--repo-suggested-reviewers ((_ git-platform-mock) _full-name)
  "The remaining workspace members, standing in for the members endpoint.
Overrides the Bitbucket implementation this class inherits, which
would otherwise hit the real network from demo/test runs."
  (gp-mock--reviewer-alist (seq-drop gp-mock--users 1)))

(cl-defmethod gp--create-pull-request ((_ git-platform-mock)
                                       full-name source dest title
                                       &optional description draft
                                       close-source-branch reviewer-uuids)
  (ignore close-source-branch)
  (let* ((db (gp-mock--db))
         (id (1+ (caar (sqlite-select db "SELECT COALESCE(MAX(id),100) FROM prs"))))
         (now (gp-mock--iso)))
    (sqlite-execute db
                    "INSERT INTO prs (id, full_name, title, description, state, draft,
                       author_uuid, author_name, source_branch, source_commit,
                       dest_branch, created_on, updated_on, commit_count, diff)
                     VALUES (?,?,?,?,'OPEN',?,?,?,?,?,?,?,?,1,'')"
                    (list id full-name title (or description "")
                          (if draft 1 0)
                          gp-mock--me git-platform-mock-user-name
                          source (format "%012x" (+ 100000 (* id 7919)))
                          dest now now))
    (dolist (uuid reviewer-uuids)
      (sqlite-execute db
                      "INSERT OR IGNORE INTO participants
                         (pr_id, user_uuid, user_name, role, state)
                       VALUES (?,?,?,'REVIEWER',NULL)"
                      (list id uuid
                            (or (cdr (assoc uuid gp-mock--users)) uuid))))
    (gp--pull-request (git-platform-backend) full-name id)))

(cl-defmethod gp--set-pull-request-reviewers ((_ git-platform-mock)
                                              full-name id reviewer-ids
                                              &optional _current-ids)
  "Replace PR ID's REVIEWER participants with REVIEWER-IDS.
Mirrors Bitbucket's whole-list PUT semantics: reviewers absent from
REVIEWER-IDS are dropped.  Rows for people who already reviewed keep
their `state', so re-saving an unchanged list does not silently
discard an approval."
  (let ((db (gp-mock--db)))
    (dolist (uuid reviewer-ids)
      (sqlite-execute db
                      "INSERT OR IGNORE INTO participants
                         (pr_id, user_uuid, user_name, role, state)
                       VALUES (?,?,?,'REVIEWER',NULL)"
                      (list id uuid
                            (or (cdr (assoc uuid gp-mock--users)) uuid))))
    ;; drop the reviewers no longer wanted (leaving non-REVIEWER roles alone)
    (let ((keep (if reviewer-ids
                    (format "AND user_uuid NOT IN (%s)"
                            (mapconcat (lambda (_) "?") reviewer-ids ","))
                  "")))
      (sqlite-execute db
                      (format "DELETE FROM participants
                               WHERE pr_id = ? AND role = 'REVIEWER' %s" keep)
                      (cons id reviewer-ids)))
    (gp--pull-request (git-platform-backend) full-name id)))

;;;; Protocol: comments ----------------------------------------------------------

(cl-defmethod gp--pull-request-comments ((_ git-platform-mock) full-name id
                                         &optional max-items)
  (let ((rows (sqlite-select
               (gp-mock--db)
               (format "SELECT %s FROM comments
                        WHERE full_name = ? AND pr_id = ? AND deleted = 0
                        ORDER BY created_on, id"
                       gp-mock--comment-cols)
               (list full-name id))))
    (when max-items (setq rows (seq-take rows max-items)))
    (mapcar #'gp-mock--comment-row rows)))

(cl-defmethod gp--create-comment ((_ git-platform-mock) full-name id text
                                  &optional inline parent-id)
  (let* ((db (gp-mock--db))
         (cid (1+ (caar (sqlite-select
                         db "SELECT COALESCE(MAX(id),1000) FROM comments")))))
    (sqlite-execute db
                    "INSERT INTO comments (id, pr_id, full_name, parent_id,
                       user_uuid, user_name, content, inline_path, inline_to,
                       resolved_by, deleted, created_on)
                     VALUES (?,?,?,?,?,?,?,?,?,NULL,0,?)"
                    (list cid id full-name parent-id
                          gp-mock--me git-platform-mock-user-name text
                          (car-safe inline) (cdr-safe inline)
                          (gp-mock--iso)))
    (gp-mock--touch-pr id)
    (gp-mock--comment cid)))

(cl-defmethod gp--edit-comment ((_ git-platform-mock) full-name id comment-id text)
  (ignore full-name)
  (sqlite-execute (gp-mock--db)
                  "UPDATE comments SET content = ?, updated_on = ? WHERE id = ?"
                  (list text (gp-mock--iso) comment-id))
  (gp-mock--touch-pr id)
  (gp-mock--comment comment-id))

(cl-defmethod gp--delete-comment ((_ git-platform-mock) full-name id comment-id)
  (ignore full-name)
  (sqlite-execute (gp-mock--db)
                  "UPDATE comments SET deleted = 1 WHERE id = ?"
                  (list comment-id))
  (gp-mock--touch-pr id)
  t)

(cl-defmethod gp--resolve-comment ((_ git-platform-mock) full-name id comment-id)
  (ignore full-name)
  (sqlite-execute (gp-mock--db)
                  "UPDATE comments SET resolved_by = ? WHERE id = ?"
                  (list git-platform-mock-user-name comment-id))
  (gp-mock--touch-pr id)
  `((user (display_name . ,git-platform-mock-user-name))))

;; Reactions: the mock mirrors GitHub's model (many per user, one row
;; each) so the UI can be demoed without a network.
(cl-defmethod gp--inline-target-problem ((_ git-platform-mock) _fn _id _path _line) nil)
(cl-defmethod gp--reactions-supported-p ((_ git-platform-mock)) t)
(cl-defmethod gp--reaction-choices ((_ git-platform-mock))
  '("+1" "-1" "laugh" "confused" "heart" "hooray" "rocket" "eyes"))

(cl-defmethod gp--comment-reactions ((_ git-platform-mock) full-name comment)
  (ignore full-name)
  (mapcar (lambda (row)
            (pcase-let ((`(,content ,uuid ,name) row))
              `((content . ,content)
                (user (uuid . ,uuid) (display_name . ,name)))))
          (sqlite-select (gp-mock--db)
                         "SELECT content, user_uuid, user_name FROM reactions
                           WHERE comment_id = ? ORDER BY rowid"
                         (list (alist-get 'id comment)))))

(cl-defmethod gp--set-comment-reaction ((_ git-platform-mock) full-name comment content on)
  (ignore full-name)
  (let ((cid (alist-get 'id comment)))
    (if on
        ;; INSERT OR IGNORE keeps the add idempotent, like GitHub's 200
        (sqlite-execute (gp-mock--db)
                        "INSERT OR IGNORE INTO reactions
                           (comment_id, user_uuid, user_name, content)
                         VALUES (?, ?, ?, ?)"
                        (list cid gp-mock--me git-platform-mock-user-name content))
      (sqlite-execute (gp-mock--db)
                      "DELETE FROM reactions
                        WHERE comment_id = ? AND user_uuid = ? AND content = ?"
                      (list cid gp-mock--me content))))
  t)

(cl-defmethod gp--reopen-comment ((_ git-platform-mock) full-name id comment-id)
  (ignore full-name)
  (sqlite-execute (gp-mock--db)
                  "UPDATE comments SET resolved_by = NULL WHERE id = ?"
                  (list comment-id))
  (gp-mock--touch-pr id)
  t)

;;;; Protocol: review state and draft flag ---------------------------------------

(defun gp-mock--set-my-review (pr-id state)
  "Set my participant STATE (a string or nil) on PR-ID."
  (sqlite-execute (gp-mock--db)
                  "INSERT INTO participants (pr_id, user_uuid, user_name, role, state)
                   VALUES (?,?,?,'REVIEWER',?)
                   ON CONFLICT (pr_id, user_uuid) DO UPDATE SET state = ?"
                  (list pr-id gp-mock--me git-platform-mock-user-name
                        state state))
  (gp-mock--touch-pr pr-id))

(cl-defmethod gp--approve-pr ((_ git-platform-mock) full-name id
                              &optional unapprove reason)
  (ignore full-name reason)
  (gp-mock--set-my-review id (unless unapprove "approved"))
  t)

(cl-defmethod gp--request-changes-pr ((_ git-platform-mock) full-name id
                                      &optional unrequest reason)
  (ignore full-name reason)
  (gp-mock--set-my-review id (unless unrequest "changes_requested"))
  t)

(cl-defmethod gp--review-retraction-kind ((_ git-platform-mock))
  'retract)

(cl-defmethod gp--set-pull-request-draft ((_ git-platform-mock) full-name id draft
                                          &optional title)
  (sqlite-execute (gp-mock--db)
                  "UPDATE prs SET draft = ?, updated_on = ? WHERE id = ?"
                  (list (if draft 1 0) (gp-mock--iso) id))
  (when title
    (sqlite-execute (gp-mock--db) "UPDATE prs SET title = ? WHERE id = ?"
                    (list title id)))
  (gp--pull-request (git-platform-backend) full-name id))

(cl-defmethod gp--set-pull-request-title ((_ git-platform-mock) full-name id title)
  (sqlite-execute (gp-mock--db)
                  "UPDATE prs SET title = ?, updated_on = ? WHERE id = ?"
                  (list title (gp-mock--iso) id))
  (gp--pull-request (git-platform-backend) full-name id))

(cl-defmethod gp--set-pull-request-description ((_ git-platform-mock) full-name id
                                                description &optional _title)
  (sqlite-execute (gp-mock--db)
                  "UPDATE prs SET description = ?, updated_on = ? WHERE id = ?"
                  (list (or description "") (gp-mock--iso) id))
  (gp--pull-request (git-platform-backend) full-name id))

;;;; Protocol: diff and stats -----------------------------------------------------

(defun gp-mock--diff (full-name id)
  "Return the stored unified diff for PR FULL-NAME/ID, or nil."
  (caar (sqlite-select (gp-mock--db)
                       "SELECT diff FROM prs WHERE full_name = ? AND id = ?"
                       (list full-name id))))

(cl-defmethod gp--pull-request-diff ((_ git-platform-mock) full-name id
                                     &optional _commit)
  (gp-mock--diff full-name id))

(defun gp-mock--chunk-stats (path chunk)
  "Return the (:path :status :added :removed) plist for one diff CHUNK."
  (let ((added 0) (removed 0))
    (dolist (line (split-string chunk "\n"))
      (cond
       ((string-prefix-p "+++" line) nil)
       ((string-prefix-p "---" line) nil)
       ((string-prefix-p "+" line) (cl-incf added))
       ((string-prefix-p "-" line) (cl-incf removed))))
    (list :path path
          :status (cond ((string-match-p "^--- /dev/null" chunk) "added")
                        ((string-match-p "^\\+\\+\\+ /dev/null" chunk) "removed")
                        (t "modified"))
          :added added :removed removed)))

(cl-defmethod gp--pull-request-stats ((_ git-platform-mock) full-name id
                                      &optional _pr)
  (let* ((row (car (sqlite-select
                    (gp-mock--db)
                    "SELECT diff, commit_count FROM prs
                     WHERE full_name = ? AND id = ?"
                    (list full-name id))))
         (files (mapcar (pcase-lambda (`(,path . ,chunk))
                          (gp-mock--chunk-stats path chunk))
                        (gp-split-diff-by-file (or (car row) "")))))
    (list :files (length files)
          :added (apply #'+ (mapcar (lambda (f) (plist-get f :added)) files))
          :removed (apply #'+ (mapcar (lambda (f) (plist-get f :removed)) files))
          :commits (or (cadr row) 0)
          :file-list files)))

(cl-defmethod gp--pull-request-stats-async ((backend git-platform-mock) full-name id pr callback)
  ;; local sqlite, so there is nothing to wait on -- a short timer keeps the
  ;; detail view's async ordering (spinner, then fold-in) exercised
  (let ((stats (gp--pull-request-stats backend full-name id pr)))
    (run-at-time 0.2 nil callback stats)))

(cl-defmethod gp--pull-request-diff-async ((backend git-platform-mock) full-name id commit _pr callback)
  (let ((diff (gp--pull-request-diff backend full-name id commit)))
    (run-at-time 0.2 nil callback diff)))

;;;; Simulated pipelines -----------------------------------------------------------

;; PR #101 runs a live pipeline against the wall clock: Build finishes
;; ~35s after `git-platform-mock-enable', unit tests tick with a live
;; duration until ~90s, then everything is green.  PR #104 pauses at an
;; open manual gate; running the step "deploys to production" over ~20s.
;; PR #103 has a finished, failed run with a readable step log.

(defvar gp-mock--epoch nil
  "Wall-clock start of the pipeline simulation (set on enable/reset).")

(defvar gp-mock--p101-stopped nil
  "Non-nil once the demo stopped PR #101's running pipeline.")

(defvar gp-mock--prod-run-at nil
  "When non-nil, time the #104 manual production step was started.")

(defun gp-mock--elapsed ()
  "Seconds since the simulation started."
  (- (float-time) (or gp-mock--epoch (float-time))))

(defun gp-mock--pipeline (uuid num commit state result-or-stage)
  "Build a Bitbucket-shaped pipeline alist."
  `((uuid . ,uuid)
    (build_number . ,num)
    (state . ((name . ,state)
              ,@(if (equal state "COMPLETED")
                    `((result (name . ,result-or-stage)))
                  `((stage (name . ,result-or-stage))))))
    (target (commit (hash . ,commit)))))

(cl-defun gp-mock--step (uuid name state &key result duration started manual)
  "Build a Bitbucket-shaped pipeline step alist."
  `((uuid . ,uuid)
    (name . ,name)
    (state . ((name . ,state)
              ,@(when result `((result (name . ,result))))
              ,@(when (and manual (equal state "PENDING"))
                  '((stage (name . "PAUSED"))))))
    ,@(when duration `((duration_in_seconds . ,duration)))
    ,@(when started `((started_on . ,started)))
    (trigger (type . ,(if manual
                          "pipeline_step_trigger_manual"
                        "pipeline_step_trigger_automatic")))))

(defun gp-mock--p101-phase ()
  "Return #101's phase: `building', `testing', `done' or `stopped'."
  (cond (gp-mock--p101-stopped 'stopped)
        ((< (gp-mock--elapsed) 35) 'building)
        ((< (gp-mock--elapsed) 90) 'testing)
        (t 'done)))

(defun gp-mock--p101 (commit)
  "Return #101's current-commit pipeline for COMMIT."
  (pcase (gp-mock--p101-phase)
    ('done    (gp-mock--pipeline "{mock-pipe-101}" 42 commit
                                 "COMPLETED" "SUCCESSFUL"))
    ('stopped (gp-mock--pipeline "{mock-pipe-101}" 42 commit
                                 "COMPLETED" "STOPPED"))
    (_        (gp-mock--pipeline "{mock-pipe-101}" 42 commit
                                 "IN_PROGRESS" "RUNNING"))))

(defun gp-mock--p101-steps ()
  "Return #101's steps for the current simulation phase."
  (let ((build-start (gp-mock--iso-at gp-mock--epoch))
        (test-start (gp-mock--iso-at (time-add gp-mock--epoch 35))))
    (pcase (gp-mock--p101-phase)
      ('building
       (list (gp-mock--step "{mock-step-build}" "Build & lint" "IN_PROGRESS"
                            :started build-start)
             (gp-mock--step "{mock-step-test}" "Unit tests" "PENDING")
             (gp-mock--step "{mock-step-preview}" "Deploy preview" "PENDING")))
      ('testing
       (list (gp-mock--step "{mock-step-build}" "Build & lint" "COMPLETED"
                            :result "SUCCESSFUL" :duration 34)
             (gp-mock--step "{mock-step-test}" "Unit tests" "IN_PROGRESS"
                            :started test-start)
             (gp-mock--step "{mock-step-preview}" "Deploy preview" "PENDING")))
      ('stopped
       (list (gp-mock--step "{mock-step-build}" "Build & lint" "COMPLETED"
                            :result "SUCCESSFUL" :duration 34)
             (gp-mock--step "{mock-step-test}" "Unit tests" "COMPLETED"
                            :result "STOPPED")
             (gp-mock--step "{mock-step-preview}" "Deploy preview" "COMPLETED"
                            :result "STOPPED")))
      ('done
       (list (gp-mock--step "{mock-step-build}" "Build & lint" "COMPLETED"
                            :result "SUCCESSFUL" :duration 34)
             (gp-mock--step "{mock-step-test}" "Unit tests" "COMPLETED"
                            :result "SUCCESSFUL" :duration 52)
             (gp-mock--step "{mock-step-preview}" "Deploy preview" "COMPLETED"
                            :result "SUCCESSFUL" :duration 11))))))

(defun gp-mock--p104 (commit)
  "Return #104's pipeline for COMMIT: gated, running, or done."
  (cond
   ((null gp-mock--prod-run-at)
    (gp-mock--pipeline "{mock-pipe-104}" 231 commit "IN_PROGRESS" "RUNNING"))
   ((< (- (float-time) gp-mock--prod-run-at) 20)
    (gp-mock--pipeline "{mock-pipe-104}" 231 commit "IN_PROGRESS" "RUNNING"))
   (t (gp-mock--pipeline "{mock-pipe-104}" 231 commit
                         "COMPLETED" "SUCCESSFUL"))))

(defun gp-mock--p104-steps ()
  "Return #104's steps: the production apply is a manual gate."
  (append
   (list (gp-mock--step "{mock-step-plan}" "Terraform plan" "COMPLETED"
                        :result "SUCCESSFUL" :duration 58)
         (gp-mock--step "{mock-step-staging}" "Apply staging" "COMPLETED"
                        :result "SUCCESSFUL" :duration 112))
   (list
    (cond
     ((null gp-mock--prod-run-at)
      (gp-mock--step "{mock-step-prod}" "Apply production" "PENDING"
                     :manual t))
     ((< (- (float-time) gp-mock--prod-run-at) 20)
      (gp-mock--step "{mock-step-prod}" "Apply production" "IN_PROGRESS"
                     :manual t
                     :started (gp-mock--iso-at gp-mock--prod-run-at)))
     (t
      (gp-mock--step "{mock-step-prod}" "Apply production" "COMPLETED"
                     :manual t :result "SUCCESSFUL" :duration 19))))))

(defun gp-mock--pipelines-for-pr (id commit)
  "Return the mock pipelines for PR ID with head COMMIT, newest first."
  (pcase id
    (101 (list (gp-mock--p101 commit)
               (gp-mock--pipeline "{mock-pipe-101-prev}" 41
                                  "9d2b7c31e0aa" "COMPLETED" "FAILED")))
    (102 (list (gp-mock--pipeline "{mock-pipe-102}" 17 commit
                                  "COMPLETED" "SUCCESSFUL")))
    (103 (list (gp-mock--pipeline "{mock-pipe-103}" 88 commit
                                  "COMPLETED" "FAILED")))
    (104 (list (gp-mock--p104 commit)))
    (105 (list (gp-mock--pipeline "{mock-pipe-105}" 9 commit
                                  "COMPLETED" "SUCCESSFUL")))
    (_ nil)))

(cl-defmethod gp--pipelines-for-branch ((_ git-platform-mock) full-name branch
                                        &optional max-items commit)
  (let* ((row (car (sqlite-select
                    (gp-mock--db)
                    "SELECT id, source_commit FROM prs
                     WHERE full_name = ? AND source_branch = ?"
                    (list full-name branch))))
         (pipelines (and row (gp-mock--pipelines-for-pr (car row) (cadr row)))))
    (when max-items (setq pipelines (seq-take pipelines max-items)))
    (bitbucket-pipelines-match-commit pipelines commit)))

;; The mock reads a local sqlite db, so there is nothing to defer: the async
;; twins just answer straight away.  Callers must already tolerate a
;; synchronous callback (a warm commit-message cache does the same live).
(cl-defmethod gp--pipelines-for-branch-async ((backend git-platform-mock) full-name branch
                                              max-items commit callback)
  (funcall callback
           (gp--pipelines-for-branch backend full-name branch max-items commit)))

(cl-defmethod gp--pipeline-steps-async ((backend git-platform-mock) full-name pipeline-uuid callback)
  (funcall callback (gp--pipeline-steps backend full-name pipeline-uuid)))

(cl-defmethod gp--commit-message-async ((backend git-platform-mock) full-name hash callback)
  (funcall callback (gp--commit-message backend full-name hash)))

(cl-defmethod gp--pipeline-steps ((_ git-platform-mock) _full-name pipeline-uuid)
  (pcase pipeline-uuid
    ("{mock-pipe-101}" (gp-mock--p101-steps))
    ("{mock-pipe-101-prev}"
     (list (gp-mock--step "{mock-step-prev-build}" "Build & lint" "COMPLETED"
                          :result "SUCCESSFUL" :duration 36)
           (gp-mock--step "{mock-step-prev-test}" "Unit tests" "COMPLETED"
                          :result "FAILED" :duration 48)))
    ("{mock-pipe-102}"
     (list (gp-mock--step "{mock-step-102-build}" "Build" "COMPLETED"
                          :result "SUCCESSFUL" :duration 21)
           (gp-mock--step "{mock-step-102-test}" "Tests" "COMPLETED"
                          :result "SUCCESSFUL" :duration 47)))
    ("{mock-pipe-103}"
     (list (gp-mock--step "{mock-step-103-build}" "Build" "COMPLETED"
                          :result "SUCCESSFUL" :duration 41)
           (gp-mock--step "{mock-step-103-test}" "Unit tests" "COMPLETED"
                          :result "FAILED" :duration 23)
           (gp-mock--step "{mock-step-103-deploy}" "Deploy" "NOT_RUN")))
    ("{mock-pipe-104}" (gp-mock--p104-steps))
    (_ nil)))

(defconst gp-mock--test-log-lines
  '("$ pytest tests/ -x --no-header"
    "collected 214 items"
    ""
    "tests/test_cart.py ................            [  7%]"
    "tests/test_catalog.py .......................  [ 18%]"
    "tests/test_checkout.py ..................      [ 27%]"
    "tests/test_giftcard.py ...                     [ 28%]"
    "tests/test_inventory.py ..................     [ 36%]"
    "tests/test_orders.py ....................      [ 46%]"
    "tests/test_pricing.py ..................       [ 54%]"
    "tests/test_search.py ......................    [ 65%]"
    "tests/test_shipping.py ..................      [ 73%]"
    "tests/test_users.py ..................         [ 82%]"
    "tests/test_webhooks.py ..............          [ 88%]"
    "tests/test_api.py ....................         [100%]"
    ""
    "============ 214 passed in 51.80s ============")
  "The #101 unit-test log, revealed line by line while the step runs.")

(defun gp-mock--live-test-log ()
  "Return the #101 test log up to the current simulation time."
  (let* ((done (memq (gp-mock--p101-phase) '(done stopped)))
         (n (if done
                (length gp-mock--test-log-lines)
              (max 1 (floor (/ (- (gp-mock--elapsed) 35) 3.5))))))
    (string-join (seq-take gp-mock--test-log-lines n) "\n")))

(cl-defmethod gp--pipeline-step-log-classify-line ((_ git-platform-mock) line)
  ;; Mock logs use a "$ " command echo (distinct from Bitbucket's "+ ", so a
  ;; test asserting on the real backend's prefix can't pass against the mock
  ;; by accident).
  (if (string-prefix-p "$ " line)
      (cons 'command (substring line 2))
    (cons nil line)))

(cl-defmethod gp--pipeline-step-log ((_ git-platform-mock) _full-name
                                     _pipeline-uuid step-uuid)
  (pcase step-uuid
    ("{mock-step-test}" (gp-mock--live-test-log))
    ("{mock-step-build}"
     "$ ruff check src/ && python -m build\nAll checks passed!\nSuccessfully built webshop-2.14.0-py3-none-any.whl")
    ("{mock-step-103-test}"
     (concat
      "$ pytest tests/billing -x\n"
      "collected 96 items\n\n"
      "tests/billing/test_invoice.py .......F\n\n"
      "=================== FAILURES ===================\n"
      "________ test_multi_currency_total_rounds_once ________\n\n"
      "    def test_multi_currency_total_rounds_once():\n"
      ">       assert invoice_total(inv) == Decimal(\"107.18\")\n"
      "E       AssertionError: assert Decimal('107.17') == Decimal('107.18')\n\n"
      "tests/billing/test_invoice.py:44: AssertionError\n"
      "========= 1 failed, 7 passed in 4.21s =========="))
    ("{mock-step-plan}"
     "$ terraform plan -out=tfplan\nPlan: 3 to add, 2 to change, 0 to destroy.")
    ("{mock-step-staging}"
     "$ terraform apply tfplan\nApply complete! Resources: 3 added, 2 changed, 0 destroyed.\ncluster version: 1.30")
    ("{mock-step-prod}"
     (if gp-mock--prod-run-at
         "$ terraform apply tfplan\nApply complete! Resources: 3 added, 2 changed, 0 destroyed.\ncluster version: 1.30"
       ""))
    (_ "(no log captured)")))

(cl-defmethod gp--pipeline-stop ((_ git-platform-mock) _full-name pipeline-uuid)
  (when (equal pipeline-uuid "{mock-pipe-101}")
    (setq gp-mock--p101-stopped (float-time)))
  t)

(cl-defmethod gp--pipeline-trigger ((_ git-platform-mock) full-name branch
                                    &optional _selector _variables)
  (message "(mock) pipeline triggered for %s on %s" full-name branch)
  '((uuid . "{mock-pipe-triggered}")))

(cl-defmethod gp--pipeline-run-manual-step ((_ git-platform-mock)
                                            _full-name _branch _pipeline step)
  (if (equal (alist-get 'uuid step) "{mock-step-prod}")
      (progn (setq gp-mock--prod-run-at (float-time)) t)
    (message "(mock) manual step %S started" (alist-get 'name step))
    t))

(defconst gp-mock--commit-messages
  '(("9d2b7c31e0aa" . "Add gift card model and migrations")
    ("3f9c2ab1d4e5" . "Add gift card support to checkout")
    ("c1d2e3f4a5b6" . "Fix invoice rounding for multi-currency accounts")
    ("0ab1c2d3e4f5" . "Upgrade EKS clusters to 1.30"))
  "Commit hash -> message, for the recent-pipelines summaries.")

(cl-defmethod gp--commit-message ((_ git-platform-mock) _full-name hash)
  (or (cdr (assoc hash gp-mock--commit-messages)) "Demo commit"))

(cl-defmethod gp--pull-request-commits-async ((_ git-platform-mock) full-name id callback
                                              &optional max-items)
  "Synthesize a PR's commit list from the mock db.
The newest entry is the PR's real head commit (so the detail view's
commits and pipelines agree on it); older ones are derived filler,
enough to exercise rendering and the max-items cap."
  (let* ((row (car (sqlite-select
                    (gp-mock--db)
                    "SELECT source_commit, commit_count, updated_on FROM prs
                     WHERE full_name = ? AND id = ?"
                    (list full-name id))))
         (head (nth 0 row))
         (n (max 1 (or (nth 1 row) 1)))
         (commits '()))
    (dotimes (i n)
      (let ((hash (if (zerop i)
                      head
                    ;; stable, obviously-fake hashes for the older entries
                    (format "%012x" (+ #xc0ffee00 (* 4099 i))))))
        (push (list :hash hash
                    :summary (if (zerop i)
                                 (gp--commit-message (git-platform-backend) full-name hash)
                               (format "Earlier work on this branch (%d)" i))
                    :author (if (cl-evenp i) "Ada Lovelace" "Grace Hopper")
                    :date (nth 2 row))
              commits)))
    (setq commits (nreverse commits))
    (funcall callback (if max-items (seq-take commits max-items) commits))))

(defun gp-mock--commit-statuses (hash)
  "Return the build-status strings for commit HASH across all mock PRs."
  (let (states)
    (pcase-dolist (`(,id ,commit)
                   (sqlite-select (gp-mock--db)
                                  "SELECT id, source_commit FROM prs"))
      (dolist (p (gp-mock--pipelines-for-pr id commit))
        (let ((h (bitbucket-pipeline-commit p)))
          (when (and h (or (string-prefix-p h hash) (string-prefix-p hash h)))
            (push (pcase (cons (bitbucket-pipeline-state p)
                               (bitbucket-pipeline-result p))
                    (`("COMPLETED" . "SUCCESSFUL") "SUCCESSFUL")
                    (`("COMPLETED" . "FAILED") "FAILED")
                    (`("COMPLETED" . "STOPPED") "STOPPED")
                    (_ "INPROGRESS"))
                  states)))))
    (nreverse states)))

(cl-defmethod gp--commit-build-states ((_ git-platform-mock) _full-name hash)
  (gp-mock--commit-statuses hash))

;;;; Advice: the direct bitbucket-* calls the UI makes ---------------------------

(defun git-platform-mock-active-p ()
  "Non-nil while the mock backend is the active git-platform backend."
  (cl-typep git-platform-current-backend 'git-platform-mock))

(defun gp-mock--pr-async-advice (orig full-name id callback)
  "Serve `bitbucket-pull-request-async' from the mock db when active."
  (if (git-platform-mock-active-p)
      (run-at-time 0.05 nil
                   (lambda ()
                     (let ((pr (ignore-errors (gp-pull-request full-name id))))
                       (funcall callback (and pr t) pr))))
    (funcall orig full-name id callback)))

(defun gp-mock--comments-async-advice (orig full-name id callback
                                            &optional max-items)
  "Serve `bitbucket-pull-request-comments-async' from the mock db when active."
  (if (git-platform-mock-active-p)
      (run-at-time 0.05 nil
                   (lambda ()
                     (funcall callback t
                              (ignore-errors
                                (gp-pull-request-comments full-name id
                                                          max-items)))))
    (funcall orig full-name id callback max-items)))

(defun gp-mock--get-async-advice (orig path params callback)
  "Serve the commit-statuses endpoint (helm bubbles) from the mock when active."
  (cond
   ((not (git-platform-mock-active-p))
    (funcall orig path params callback))
   ((string-match "\\`/repositories/.+/commit/\\([^/]+\\)/statuses\\'" path)
    (let ((hash (match-string 1 path)))
      (run-at-time 0.05 nil
                   (lambda ()
                     (funcall callback
                              `((values . ,(mapcar (lambda (s) `((state . ,s)))
                                                   (gp-mock--commit-statuses
                                                    hash)))))))))
   (t
    (gp-log 'info "mock: unhandled async GET %s" path)
    (funcall callback nil))))

;;;; Enable / disable / reset -----------------------------------------------------

(defvar gp-mock--saved-cache-ttl nil
  "`bitbucket-cache-ttl' before the mock shortened it, or nil.")

;;;###autoload
(defun git-platform-mock-enable (&optional reset)
  "Switch git-platform to the SQLite-backed demo backend.
With prefix argument RESET, first wipe the database and reseed it
\(for a fresh screencast take).  Undo with `git-platform-mock-disable'."
  (interactive "P")
  (when reset (git-platform-mock-reset))
  (gp-mock--db)
  (unless gp-mock--epoch (setq gp-mock--epoch (float-time)))
  (setq git-platform-current-backend (git-platform-mock))
  (advice-add 'bitbucket-pull-request-async :around
              #'gp-mock--pr-async-advice)
  (advice-add 'bitbucket-pull-request-comments-async :around
              #'gp-mock--comments-async-advice)
  (advice-add 'bitbucket-api-get-async :around
              #'gp-mock--get-async-advice)
  ;; near-zero cache so every list/detail visit sees your latest edits
  (unless gp-mock--saved-cache-ttl
    (setq gp-mock--saved-cache-ttl bitbucket-cache-ttl))
  (setq bitbucket-cache-ttl 2)
  (gp-mock--clear-ui-caches)
  (message "git-platform: MOCK backend on (%s) — try M-x gp-helm"
           (abbreviate-file-name (expand-file-name git-platform-mock-db-file))))

;;;###autoload
(defun git-platform-mock-disable ()
  "Switch back from the demo backend to the configured real backend."
  (interactive)
  (advice-remove 'bitbucket-pull-request-async #'gp-mock--pr-async-advice)
  (advice-remove 'bitbucket-pull-request-comments-async
                 #'gp-mock--comments-async-advice)
  (advice-remove 'bitbucket-api-get-async #'gp-mock--get-async-advice)
  (when gp-mock--saved-cache-ttl
    (setq bitbucket-cache-ttl gp-mock--saved-cache-ttl
          gp-mock--saved-cache-ttl nil))
  (setq git-platform-current-backend nil)   ;; rebuilt lazily from the default
  (gp-mock--clear-ui-caches)
  (message "git-platform: mock backend off"))

;;;###autoload
(defun git-platform-mock-reset ()
  "Wipe and reseed the demo database and restart the pipeline clock.
Use between screencast takes to get the exact same starting state."
  (interactive)
  (when gp-mock--db
    (ignore-errors (sqlite-close gp-mock--db))
    (setq gp-mock--db nil))
  (let ((file (expand-file-name git-platform-mock-db-file)))
    (when (file-exists-p file) (delete-file file)))
  (setq gp-mock--epoch (float-time)
        gp-mock--p101-stopped nil
        gp-mock--prod-run-at nil)
  (gp-mock--clear-ui-caches)
  (gp-mock--db)
  (message "git-platform-mock: database reseeded, pipeline clock restarted"))

(defun gp-mock--clear-ui-caches ()
  "Drop the result caches so the UI re-reads through the active backend."
  (bitbucket-cache-clear)
  (when (boundp 'gp-helm--pipeline-cache)
    (clrhash gp-helm--pipeline-cache))
  (when (boundp 'gp--comment-outdated-cache)
    (clrhash gp--comment-outdated-cache)))

;;;; Seed data ---------------------------------------------------------------------

(defconst gp-mock--diff-101 "diff --git a/src/checkout/payment.py b/src/checkout/payment.py
--- a/src/checkout/payment.py
+++ b/src/checkout/payment.py
@@ -18,9 +18,15 @@ class PaymentProcessor:
     def charge(self, order, card):
         \"\"\"Charge CARD for ORDER's total.\"\"\"
         total = order.total()
-        receipt = self.gateway.charge(card, total)
-        order.mark_paid(receipt)
-        return receipt
+        applied = giftcard.redeem(order.customer, total)
+        remainder = total - applied.amount
+        if remainder > 0:
+            receipt = self.gateway.charge(card, remainder)
+        else:
+            receipt = Receipt.zero(order)
+        receipt.giftcard = applied
+        order.mark_paid(receipt)
+        return receipt

     def refund(self, order):
         \"\"\"Refund ORDER in full.\"\"\"
diff --git a/src/checkout/giftcard.py b/src/checkout/giftcard.py
--- /dev/null
+++ b/src/checkout/giftcard.py
@@ -0,0 +1,18 @@
+\"\"\"Gift card redemption for checkout.\"\"\"
+
+from decimal import Decimal
+
+from .models import GiftCard, Redemption
+
+
+def redeem(customer, amount):
+    \"\"\"Redeem up to AMOUNT from CUSTOMER's active gift cards.
+
+    Returns a Redemption covering what the cards held; the caller
+    charges the remainder to the customer's card.
+    \"\"\"
+    cards = GiftCard.active_for(customer)
+    covered = min(amount, sum(c.balance for c in cards))
+    for card in cards:
+        card.drain(min(card.balance, covered))
+    return Redemption(customer=customer, amount=Decimal(covered))
diff --git a/tests/test_giftcard.py b/tests/test_giftcard.py
--- /dev/null
+++ b/tests/test_giftcard.py
@@ -0,0 +1,18 @@
+\"\"\"Tests for gift card redemption.\"\"\"
+
+from checkout.giftcard import redeem
+
+
+def test_redeem_partial_balance(customer_with_card):
+    redemption = redeem(customer_with_card, 50)
+    assert redemption.amount == 30
+
+
+def test_redeem_covers_full_amount(customer_with_card):
+    redemption = redeem(customer_with_card, 10)
+    assert redemption.amount == 10
+
+
+def test_redeem_ignores_expired_cards(customer_with_expired_card):
+    redemption = redeem(customer_with_expired_card, 25)
+    assert redemption.amount == 0
")

(defconst gp-mock--diff-102 "diff --git a/src/search/indexer.py b/src/search/indexer.py
--- a/src/search/indexer.py
+++ b/src/search/indexer.py
@@ -40,7 +40,6 @@ class ProductIndexer:
     def reindex(self, products):
         for p in products:
-            doc = self.build_doc(p)
-            self.client.index(doc)
+            self.client.index(self.build_doc(p))
         self.client.flush()

     def build_doc(self, product):
")

(defconst gp-mock--diff-103 "diff --git a/src/billing/invoice.py b/src/billing/invoice.py
--- a/src/billing/invoice.py
+++ b/src/billing/invoice.py
@@ -10,10 +10,9 @@ from decimal import Decimal, ROUND_HALF_UP

 def line_total(line):
     \"\"\"Return LINE's total in the invoice currency.\"\"\"
-    rate = get_rate(line.currency)
-    gross = line.qty * line.unit_price * rate
-    return round(gross, 2)
+    gross = line.qty * line.unit_price
+    return convert(gross, line.currency).quantize(
+        Decimal(\"0.01\"), rounding=ROUND_HALF_UP)


 def invoice_total(invoice):
diff --git a/src/billing/currency.py b/src/billing/currency.py
--- a/src/billing/currency.py
+++ b/src/billing/currency.py
@@ -22,6 +22,12 @@ def get_rate(currency):
     return RATES[currency]


+def convert(amount, currency):
+    \"\"\"Convert AMOUNT from CURRENCY into the invoice currency.\"\"\"
+    if currency == BASE_CURRENCY:
+        return Decimal(amount)
+    return Decimal(amount) * get_rate(currency)
+
+
 def format_money(amount, currency):
     \"\"\"Render AMOUNT for display.\"\"\"
     return f\"{amount:.2f} {currency}\"
")

(defconst gp-mock--diff-104 "diff --git a/eks/cluster.tf b/eks/cluster.tf
--- a/eks/cluster.tf
+++ b/eks/cluster.tf
@@ -3,7 +3,7 @@ resource \"aws_eks_cluster\" \"main\" {
   name     = \"acme-main\"
-  version  = \"1.29\"
+  version  = \"1.30\"
   role_arn = aws_iam_role.cluster.arn

   vpc_config {
diff --git a/eks/nodegroups.tf b/eks/nodegroups.tf
--- a/eks/nodegroups.tf
+++ b/eks/nodegroups.tf
@@ -12,7 +12,7 @@ resource \"aws_eks_node_group\" \"workers\" {
   node_group_name = \"workers\"
-  release_version = \"1.29.3-20250601\"
+  release_version = \"1.30.1-20260620\"
   instance_types  = [\"m6i.large\"]

   scaling_config {
")

(defconst gp-mock--diff-105 "diff --git a/requirements.txt b/requirements.txt
--- a/requirements.txt
+++ b/requirements.txt
@@ -1,5 +1,5 @@
-requests==2.31.0
-cryptography==42.0.5
+requests==2.32.3
+cryptography==43.0.1
 pydantic==2.7.1
 boto3==1.34.100
")

(defun gp-mock--seed (db)
  "Fill DB with the demo workspace."
  (let ((me gp-mock--me)
        (my-name git-platform-mock-user-name)
        (alice "{mock-alice}") (bob "{mock-bob}") (carol "{mock-carol}")
        (alice-name "Alice Meyer") (bob-name "Bob Tanaka")
        (carol-name "Carol Novak")
        (hour 3600) (day 86400))
    (cl-labels
        ((pr (id full title author-uuid author-name branch commit
                 &key draft (state "OPEN") created updated (commits 1)
                 (diff ""))
           (sqlite-execute
            db "INSERT INTO prs (id, full_name, title, description, state,
                  draft, author_uuid, author_name, source_branch,
                  source_commit, dest_branch, created_on, updated_on,
                  commit_count, diff)
                VALUES (?,?,?,?,?,?,?,?,?,?,'main',?,?,?,?)"
            (list id full title "" state (if draft 1 0)
                  author-uuid author-name branch commit
                  (gp-mock--iso created) (gp-mock--iso updated)
                  commits diff)))
         (part (pr-id uuid name state)
           (sqlite-execute
            db "INSERT INTO participants (pr_id, user_uuid, user_name, role, state)
                VALUES (?,?,?,'REVIEWER',?)"
            (list pr-id uuid name state)))
         (cmt (id pr-id full uuid name text ago
                  &key parent path line resolved-by)
           (sqlite-execute
            db "INSERT INTO comments (id, pr_id, full_name, parent_id,
                  user_uuid, user_name, content, inline_path, inline_to,
                  resolved_by, deleted, created_on)
                VALUES (?,?,?,?,?,?,?,?,?,?,0,?)"
            (list id pr-id full parent uuid name text path line
                  resolved-by (gp-mock--iso ago)))))

      ;; #101 -- your PR with review activity and the live pipeline
      (pr 101 "acme/webshop" "Add gift card support to checkout"
          me my-name "feature/gift-cards" "3f9c2ab1d4e5"
          :created (* 3 day) :updated (* 3 hour) :commits 4
          :diff gp-mock--diff-101)
      (part 101 alice alice-name "approved")
      (part 101 bob bob-name nil)
      (cmt 1001 101 "acme/webshop" alice alice-name
           "Really nice feature :tada: — left two small questions inline."
           (* 26 hour))
      (cmt 1002 101 "acme/webshop" alice alice-name
           "Should we validate the remaining balance before charging the card? If `redeem` races with another checkout the remainder could go negative."
           (* 26 hour) :path "src/checkout/payment.py" :line 22)
      (cmt 1003 101 "acme/webshop" me my-name
           "Good catch — `redeem` locks the card rows, so the balance can't move under us. I'll add an assertion here anyway."
           (* 24 hour) :parent 1002)
      (cmt 1004 101 "acme/webshop" bob bob-name
           "nit: prefer `Decimal` over the float sum here — `covered` can pick up binary noise before the `min()`."
           (* 20 hour) :path "src/checkout/giftcard.py" :line 15
           :resolved-by my-name)
      (cmt 1005 101 "acme/webshop" bob bob-name
           "Could we also cover the zero-balance card case?"
           (* 3 hour) :path "tests/test_giftcard.py" :line 6)

      ;; #102 -- your draft
      (pr 102 "acme/webshop" "Refactor product search indexing"
          me my-name "chore/search-indexer" "77aa12bc34de"
          :draft t :created (* 5 hour) :updated (* 5 hour)
          :diff gp-mock--diff-102)

      ;; #103 -- Alice's PR awaiting your review (outdated comment, failed CI)
      (pr 103 "acme/billing" "Fix invoice rounding for multi-currency accounts"
          alice alice-name "fix/invoice-rounding" "c1d2e3f4a5b6"
          :created (* 2 day) :updated (* 7 hour) :commits 3
          :diff gp-mock--diff-103)
      (part 103 me my-name nil)
      (part 103 carol carol-name "approved")
      (cmt 1006 103 "acme/billing" alice alice-name
           (format "@%s could you review? The rounding strategy in `line_total` changed — see `convert`." my-name)
           (* 8 hour))
      (cmt 1007 103 "acme/billing" carol carol-name
           "This rounds before summing — totals can drift by a cent per line."
           (* 30 hour) :path "src/billing/invoice.py" :line 99)
      (cmt 1008 103 "acme/billing" alice alice-name
           "Fixed in the latest push — rounding now happens once, in `convert`."
           (* 7 hour) :parent 1007)

      ;; #104 -- Bob's PR awaiting your review (manual gate)
      (pr 104 "acme/infra" "Upgrade EKS clusters to 1.30"
          bob bob-name "infra/eks-1-30" "0ab1c2d3e4f5"
          :created (* 30 hour) :updated (* 4 hour) :commits 2
          :diff gp-mock--diff-104)
      (part 104 me my-name nil)
      (part 104 alice alice-name "changes_requested")
      (cmt 1009 104 "acme/infra" bob bob-name
           "Staging applied cleanly. Holding the production apply for the maintenance window — feel free to review meanwhile."
           (* 4 hour))

      ;; #105 -- merged history (visible with C-c m)
      (pr 105 "acme/billing" "Bump security-sensitive dependencies"
          carol carol-name "chore/dep-bumps" "5e6f7a8b9c0d"
          :state "MERGED" :created (* 10 day) :updated (* 6 day)
          :diff gp-mock--diff-105)
      (part 105 alice alice-name "approved"))))

(provide 'git-platform-mock)
;;; git-platform-mock.el ends here
