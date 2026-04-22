;;; core-ai.el --- AI assistants and LLM integration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Umar Ahmad
;; Created: April 15, 2026
;; Modified: April 15, 2026
;; Version: 0.0.1
;; Author: Umar Ahmad
;;; Commentary:

;; Configuration for AI/LLM tools: gptel with multiple backends
;; (Anthropic, OpenAI, Gemini, Together, Groq, Bedrock, Copilot),
;; presets (coder, architect, fact, google), chat summarization,
;; agent-shell for Claude Code, and claude-code-ide.

;;; Code:

(use-package gptel
  :commands (gptel gptel-send)
  :bind (
         ;; ("C-c q s" . gptel-send)
         ;; ("C-c q r" . gptel-rewrite)
         ("C-c q c" . gptel)
         ("C-c q m" . gptel-menu)
         (:map gptel-mode-map
               ("C-M-<return>" . gptel-send)))
  :config
  (setq gptel-model 'claude-opus-4-5-20251101)
  (setq gptel-default-mode 'org-mode)
  (setq-default gptel-org-branching-context nil)
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "*** ")
  (setq gptel-api-key (secret-get openai-key))
  (require 'core-gptel-tools)
  (defun +gptel-notify(&rest _)
    (if (and gptel-tools-auto-pilot (not (frame-focus-state)))
        (alert "GPTel response completed" :title "GPTel")))
  (add-hook 'gptel-post-response-functions '+gptel-notify)
  (gptel-make-openai "TogetherAI"
    :host "api.together.xyz"
    :key (secret-get together-ai-key)
    :stream t
    :models '(;; has many more, check together.ai
              mistralai/Mixtral-8x7B-Instruct-v0.1
              codellama/CodeLlama-13b-Instruct-hf
              codellama/CodeLlama-34b-Instruct-hf))
  (gptel-make-gemini "Gemini Grounded"
    :stream t
    :key (secret-get gemini-key)
    :request-params '(:tools [(:google_search ())]))

  (gptel-make-bedrock "AWS"
    :stream t
    :region "us-east-1"
    :aws-bearer-token (secret-get aws-bedrock-key)
    :models '(claude-sonnet-4-20250514 claude-haiku-4-5-20251001 claude-opus-4-5-20251101)
    :model-region 'us)

  (gptel-make-openai "Groq"
    :host "api.groq.com"
    :endpoint "/openai/v1/chat/completions"
    :stream t
    :key (secret-get groq-key)
    :models '(llama-3.3-70b-versatile
              llama3-70b-8192
              deepseek-r1-distill-qwen-32b
              deepseek-r1-distill-llama-70b))
  (gptel-make-gh-copilot "Copilot")

  (defun gptel-project-conventions ()
    "System prompt is obtained from project conventions files.

Looks for CONVENTIONS.md, then CLAUDE.md, then AGENTS.md at the project root."
    (when-let* ((root (project-root (project-current))))
      (let* ((candidates '("CONVENTIONS.md" "CLAUDE.md" "AGENTS.md"))
             (file (seq-find (lambda (name)
                               (file-readable-p (file-name-concat root name)))
                             candidates)))
        (if file
            (with-temp-buffer
              (insert-file-contents (file-name-concat root file))
              (buffer-string))
          ""))))


  (defun gptel-default-prompt ()
    "Return the default system prompt including project conventions and context."
    (let* ((base "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
           (conventions (when (fboundp 'gptel-project-conventions)
                          (gptel-project-conventions)))
           (root (when-let ((proj (project-current nil)))
                   (project-root proj))))
      (string-join
       (delq nil
             (list
              base
              (when (and (stringp conventions)
                         (not (string-empty-p conventions)))
                (concat "Conventions for this project:\n" conventions))
              (format "visible buffers: %s"
                      (if (fboundp 'gptel-tool-list-visible-buffers)
                          (gptel-tool-list-visible-buffers)
                        "(gptel-tool-list-visible-buffers unavailable)"))
              (concat "Current date and time is: " (format-time-string "%Y-%m-%d %H:%M:%S"))
              (concat "Current directory: " (or default-directory ""))
              (concat "Current project: " (or root ""))))
       "\n\n")))

  (setf (alist-get 'default gptel-directives) #'gptel-default-prompt)

  (gptel-make-preset 'coder
    :description "Preset for coding tasks"
    :backend "Anthropic-OAuth"
    :system 'default
    :model 'claude-opus-4-5-20251101
    :tools
    '("run_command" "get_outline"
      "list_errors" "edit_buffer"
      "list_project_files"
      "read_file" "search_with_ripgrep" "create_file"
      "eval_elisp" "web_search" "web_fetch"))
  (gptel-make-preset 'architect
    :description "Preset for spec writer"
    :backend "ChatGPT"
    :model 'gpt-5
    :system (concat "You are an expert technical architect and product expert who is working on writing a spec in org-mode format.\n"
                    "Your first task would be identify the buffer which you would be editing in.\n"
                    "All spec files are usually situated as a single org file in the specs directory inside the project directory.\n\n"
                    "You'll only edit this org-file and only respond in chat if there is confusion or if you have a question or if user specifically asks to discuss a particular point. In all other cases only edit the spec file.\n"
                    "All code blocks should be in org-mode format of:\n"
                    "#+begin_src language\n"
                    "#+end_src\n"
                    "Proper org-mode headings sub-headings sub-sub-headings and so are used wherever needed.\n"
                    "inline code blocks should be written with either =this markup= or ~this~\n"
                    "When referring to a file on a line number in the response, use the format [[file:<full_file_name>::<line_number>][<file_base_name>::<line_number>]]\n"
                    "When an existing spec refers to a file, you might as well want to read that file to understand the context.\n"
                    "When editing the spec, don't change parts of the spec you want to be unchanged.\n\n"
                    "Your second task would then be to figure out what part of the spec we're currently working on.\n"
                    "We work in this order Requirement -> Design -> Tasks\n\n"
                    "Requirements explain the complete user story and what components are required\n"
                    "They also list non user aspects such as tests, compliance, performance etc.\n"
                    "Requirements also has a open questions section that you can fill with on things that are still unclear in the requirement\n\n"
                    "Design is a more detailed take on the requirements.\n"
                    "All components, technologies and frameworks, directory structures, build technique, deployment, UI frameworks, UI components, queries, schemas, working functions for major components of code are written here. Leave no part open.\n"
                    "The design should also have a section of \"Handle later\" if required.\n"
                    "This lists out all the things that were discussed but were deferred for later.\n\n"
                    "Tasks is a checklists of all the task that need to be done. The task will have multiple sub-sections that group one type of sequential tasks and have a org-mode checklist style.\n"
                    "Eg:\n"
                    "*** 1. Setting up the server\n"
                    "1. [X] pull docker image by running =docker pull=\n"
                    "2. [X] Write a Dockerfile for the service\n"
                    "3. [ ] Create directory structure as given in design\n\n"
                    "and so on.\n"
                    "Every group should also have as a last task a task that validates that the group is complete, by actually running the application or querying data using tools\n"
                    "We can later refer to tasks like 1.2 to refer to task 2 in section 1.\n\n"
                    "The task list would be completely exhaustive. This means if all the tasks are done, the requirements are complete according to the design. \n"
                    "Don't add any tasks that have been mentioned in requirements or design to be done later\n")
    :tools '("run_command" "edit_buffer" "list_visible_buffers" "list_buffers"
             "list_project_files" "find_apropos"
             "find_definitions" "find_references" "change_directory" "list_projects" "read_file"
             "search_with_ripgrep" "list_directory" "make_directory" "open_file_in_background"
             "create_file" "delete_file" "eval_elisp"))



  (gptel-make-preset 'fact
    :description "Preset for answering questions on current context without tools"
    :backend "ChatGPT"
    :system (lambda()
              (concat "You are an expert analyst and researcher. "
                      "You use web search and web fetch to tools to answer questions. "
                      "you also do deep research, going through multiple sources, on the topic before answering. "
                      "You also try to find opposing information in your research to get a balanced view. "
                      "If you don't know the answer or can't find relevant information, you admit it. "
                      "Be precise in your answers and do not reply in long paragraphs. "
                      "You always give sources of the information you provide. "
                      "For links format would be [[https://example.com/full/web-page-url/][Web page title]].\n\n"
                      "Current date and time is: " (format-time-string "%Y-%m-%d %H:%M:%S")))
    :model 'gpt-4o-mini
    :tools '("web_search" "web_fetch" "remember" "list_tags" "get_tag" "search_memory"))

  (gptel-make-preset 'google
    :description "Preset for Google search"
    :backend "Gemini Grounded"
    :model 'gemini-2.5-flash
    :tools nil
    :include-reasoning nil
    :system (concat "You are an LLM agent running inside Emacs."
                    "You acting as expert analyst and researcher with access to google search. "
                    "You always google search your answers and answer based on latest data. "
                    "Be precise in your answers and do not reply in long paragraphs. "
                    "You alwasy give links to the source of the information you provide."
                    "The link format would be [[https://example.com/full/web-page-url/][Web page title]]"))

  (defun gptel-summarize-chat ()
    "Summarize chat conversations and replace the current buffer content with the summary."
    (interactive)
    (let* ((orig-buffer (current-buffer))
           (gptel-backend (alist-get "ChatGPT" gptel--known-backends
                                     nil nil #'equal))
           (gptel-model "gpt-4.1"))
      (gptel-request (concat (buffer-string)
                             "\n\nSummarize in detail this partial conversation about programming.\n"
                             "Include less detail about older parts and more detail about the most recent messages.\n"
                             "Incase the chat starts with a recap such as \"I spoke to you previously...\", then include as much detail from it as possible.\n"
                             "Start a new paragraph every time the topic changes!\n\n"
                             "This is only part of a longer conversation so *DO NOT* conclude the summary with language like \"Finally, ...\". Because the conversation continues after the summary.\n"
                             "The summary *MUST* include the function names, libraries, packages, project directories that are being discussed.\n"
                             "The summary *MUST* include the filenames and buffer names that are being referenced by the assistant inside the =...= fenced code blocks!\n"
                             "The summary *MUST* include the important learnings and the course of the conversation.\n"
                             "The summaries *MUST NOT* include detailed code blocks!\n\n"
                             "The summaries *MUST NOT* include file contents that you read but only file / buffer names!\n\n"
                             "The summaries *SHOULD* include specific line numbers in buffer and files that you referred to in the course of conversation!\n\n"
                             "Phrase the summary with the USER in first person, telling the ASSISTANT about the conversation.\n"
                             "Write *as* the user.\n"
                             "The user should refer to the assistant as *you*.\n"
                             "Start the summary with \"I asked you...\"\n\n")
        :system  "Summarize this LLM conversation transcript. Follow the summarization instructions at the bottom very carefully."
        :callback (lambda (response info)
                    (if response
                        (with-current-buffer orig-buffer
                          (let ((inhibit-read-only t))
                            (save-excursion
                              (goto-char (point-min))
                              (delete-region (point-min) (point-max))
                              (insert "*** I spoke to you previously about a number of things.\n\n" response)
                              (fill-region (point-min) (point-max))
                              (goto-char (point-min)))
                            (message "Chat summarized successfully")))
                      (message "Response failed with status: %S" (prin1-to-string info))))))))

(use-package gptel-anthropic-oauth
  :after (gptel)
  :demand t
  :ensure (:fetcher github :repo "gleek/gptel-anthropic-oauth" :protocol ssh)
  :config
  (gptel-make-anthropic-oauth "Anthropic-OAuth" :stream t))


(use-package claude-code-ide
  :ensure (:fetcher github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c q ." . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup))


(use-package agent-shell
  :bind ("C-c q a" . agent-shell)
  :config
  (setq agent-shell-prefer-session-resume nil) ; this is not recommended but I've yet to experience the slowness
  (setq agent-shell-preferred-agent-config (agent-shell-anthropic-make-claude-code-config))
  (setq agent-shell-permission-responder-function
        #'agent-shell-permission-allow-always)
  (advice-add 'shell-maker-welcome-message :override (lambda (&rest _) ""))
  (defun +agent-shell-self-insert-or-queue ()
    "Insert character normally, or queue a request if the shell is busy."
    (interactive)
    (if (shell-maker-busy)
        (let ((char (string last-command-event)))
          (agent-shell-queue-request
           (read-string (or (map-nested-elt (agent-shell--state) '(:agent-config :shell-prompt))
                            "Enqueue request: ")
                        char)))
      (self-insert-command 1)))
  (keymap-set agent-shell-mode-map "<remap> <self-insert-command>" #'+agent-shell-self-insert-or-queue)

  (define-advice agent-shell--initiate-new-session (:around (orig-fn &rest args) defer-new-session)
    "Show prompt immediately on first call, then pass through."
    (if (local-variable-p 'agent-shell--deferred-new-session-p)
        (progn
          (kill-local-variable 'agent-shell--deferred-new-session-p)
          (when agent-shell-show-busy-indicator
            (agent-shell-heartbeat-start
             :heartbeat (map-elt agent-shell--state :heartbeat)))
          (apply orig-fn args))
      (setq-local agent-shell--deferred-new-session-p t)
      (setq-local agent-shell-session-strategy 'new-deferred)
      (agent-shell-heartbeat-stop
       :heartbeat (map-elt agent-shell--state :heartbeat))
      (shell-maker-finish-output :config shell-maker--config
                                 :success nil)
      (agent-shell--emit-event :event 'prompt-ready))))

(use-package agent-shell-attention
  :ensure (:host github :repo "ultronozm/agent-shell-attention.el")
  :after agent-shell
  :demand
  :config
  (setopt agent-shell-attention-notify-function
          (lambda (_buffer title body)
            (alert body :title title :icon "nf-cod-bot")))
  (setopt agent-shell-attention-render-function
          #'agent-shell-attention-render-active)
  (setopt agent-shell-attention-indicator-location 'global-mode-string)
  (agent-shell-attention-mode))

(use-package agent-recall
  :ensure (:fetcher github :repo "Marx-A00/agent-recall")
  :after agent-shell
  :hook (agent-shell-mode-hook . agent-recall-track-sessions)
  :demand
  :bind ("C-c q s" . agent-recall-search-live)
  :config
  (setq agent-recall-search-paths
        (mapcar #'directory-file-name (projectile-relevant-known-projects)) ;; This is one time only for reindexing.
        agent-recall-max-depth 1
        agent-recall-search-function 'consult-ripgrep
        agent-recall-browse-sort 'modified-desc))

(use-package copilot
  :ensure (:fetcher github :repo "copilot-emacs/copilot.el")
  :commands (copilot-login copilot-diagnose)
  :bind (;; ("C-c M-f" . copilot-complete)
         :map copilot-completion-map
         ("C-g" . 'copilot-clear-overlay)
         ("M-p" . 'copilot-previous-completion)
         ("M-n" . 'copilot-next-completion)
         ("<backtab>" . 'copilot-accept-completion)
         ("M-F" . 'copilot-accept-completion-by-word)
         ("C-S-e" . 'copilot-accept-completion-by-line))
  :hook ((prog-mode . copilot-mode)
         ;; (text-mode . copilot-mode)
         (conf-mode . copilot-mode))
  :config
  (setq copilot-install-dir (expand-file-name "copilot" CACHE-DIR))
  (setq copilot-indent-offset-warning-disable t
        copilot-max-char-warning-disable t
        copilot-max-char 100000))

(use-package copilot-chat
  :bind (("C-c q o" . copilot-chat-display)
         ("C-c q p" . copilot-chat-custom-prompt-selection))
  :config
  (setq copilot-chat-github-token-file (concat CACHE-DIR "copilot-chat/github-token")
        copilot-chat-token-cache (concat CACHE-DIR "copilot-chat/token"))
  (setq shell-maker-root-path CACHE-DIR)

  (setopt copilot-chat-default-model "claude-3.7-sonnet")
  (setopt copilot-chat-frontend 'shell-maker))

(use-package aidermacs
  :disabled t
  ;; :bind ("C-c q a" . aidermacs-transient-menu)
  :ensure (:fetcher github :repo "MatthewZMD/aidermacs")
  :config
  ;; (setq aidermacs-default-model "anthropic/claude-3-5-sonnet-20241022")
  (setq aidermacs-default-model "o3-mini")
  (setq aidermacs-editor-model "o3-mini")
  (setq aidermacs-architect-model "o1-mini")
  (setenv "OPENAI_API_KEY" (secret-get openai-key))
  (setenv "AIDER_CHAT_LANGUAGE" "english")
  (setq aidermacs-use-architect-mode t))



(provide 'core-ai)
;;; core-ai.el ends here
