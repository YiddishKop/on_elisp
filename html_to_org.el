;; 我的目标是:
;; 1. 把所有 html 文件转换为 org 文件
;;        1.1 无法直接转换,现有技术只能 html->md->org
;; 2. 整理生成的 org 文件,主要是
;;        2.1 删除大量 #+BEGIN_HTML block
;;        2.2 更改 #+BEGIN_EXAMPLE block 为 python src block
;; 3. 删除 html 和 md 文件,仅保留 org 文件
;;
;; 本代码所依据的目录结构
;;
;; </home/yiddi/git_repos/on_ml_wushanghong/ml/
;;                                             competitions/
;;                                                         01_News-Popularity/xxx.html
;;                                                         01_Response_Selection/xxx.html
;;                                                         02_Image-Caption/xxx.html
;;                                                         02_Object_Detection/xxx.html
;;                                                         03_Image-Caption/xxx.html
;;                                                         04_Reverse-Image-Caption/xxx.html
;;                                                         05_Deep_Reinforcement_Learning/xxx.html
;;                                             labs/
;;                                                         01_Scientific-Python-101/xxx.html
;;                                                         02_EDA_PCA/xxx.html
;;                                                         03_Decision-Trees_Random-Forest/xxx.html
;;                                                         04-1_Perceptron_Adaline/xxx.html
;;                                                         04-2_Regression/xxx.html
;;                                                         05_Regularization/xxx.html
;;                                                         06_Logistic-Regression_Metrics/xxx.html
;;                                                         07_SVM_Pipeline/xxx.html
;;                                                         08_CV_Ensembling/xxx.html
;;                                                         10_TensorFlow101_Word2Vec/xxx.html
;;                                                         11_NN_Regularization/xxx.html
;;                                                         12-1_CNN/xxx.html
;;                                                         12-2_Visualization_and_Style_Transfer/xxx.html
;;                                                         13_Sentiment_Analysis_and_Neural_Machine_Translation/xxx.html
;;                                                         14_Autoencoder_GANs/xxx.html
;;                                                         16-1_Q-Learning/xxx.html
;;                                                         17_DQN_Policy_Network/xxx.html





;; 执行命令 from html to md, then to org

;; # from html to md
;; pandoc test.html -o test.md

;; # from jn to other format, eg markdown
;; jupyter nbconvert --to FORMAT notebook.ipynb

;; # from md to org
;; pandoc -f markdown -t org -o newfile.org original-file.markdown

;; Known bugs:
;; 对于 html 文档标题中带有 shell command 符号的文件转换会出现bug
;; 对于 超过 3M 的html 文档的转换会使得 pandoc 出现问题, 参考这里: https://github.com/jgm/pandoc/issues/352


(defun htmltoorg (lecdir)
  "convert all .html files under lecdir directory to org files"
  (dolist (htmlfile (directory-files lecdir t "html"))
    (progn
      (setq pathwithoutext (file-name-sans-extension htmlfile))
      (setq mdfile (concat pathwithoutext ".md"))
      (setq orgfile (concat pathwithoutext ".org"))
      (setq htmltomd (format "pandoc +RTS -K10240000 -RTS %s -o %s" htmlfile mdfile))
      (message htmltomd)
      (message (shell-command-to-string htmltomd))
      (setq mdtoorg (format "pandoc +RTS -K10240000 -RTS -f markdown -t org -o %s %s" orgfile mdfile))
      (message mdtoorg)
      (message (shell-command-to-string mdtoorg))
      )))



;; part two org 内容的改进
;; 过多 <div> tag in org files
;; 检索 #+BEGIN_HTML 检索 #+END_HTML 将两者之间包含两者所在行都删除
;; example block modify to python src block

;; replace #+BEGIN_EXAMPLE with #+BEGIN_SRC
;; replace #+END_EXAMPLE with #+END_SRC
;; 注意 replace-match 的两个可选参数是关于大小写的, 应该设置好, 随便试试就能试出来

;; combine the two progn above together
;; and add a while loop to delete empty lines
(defun del-html-modify-exmp-block (lecdir)
  "convert all .html files under lecdir directory to org files"
  (dolist (htmlfile (directory-files lecdir t "html"))
    (progn
      (setq pathwithoutext (file-name-sans-extension htmlfile))
      (setq orgfile (concat pathwithoutext ".org"))

      ;; del html block
      (set-buffer (find-file-noselect orgfile))
      (goto-char 0)
      (while (search-forward "#+BEGIN_HTML" nil t)
        (progn
          (beginning-of-line)
          (setq beginpos (point))
          (search-forward "#+END_HTML" nil t)
          (end-of-line)
          (setq endpos (point))
          (delete-region beginpos endpos)
          )
        )
      (message "delete")

      ;; modify example block
      (goto-char 0)
      (while (search-forward "#+END_EXAMPLE" nil t)
        (replace-match "#+END_SRC" nil t))
      (goto-char 0)
      (while (search-forward "#+BEGIN_EXAMPLE" nil t)
        (replace-match "#+BEGIN_SRC ipython :tangle yes :session :exports code :async t :results raw drawer" t t))

      ;; clean multiple empty lines with only 1 empty line
      (goto-char 0)
      (while (re-search-forward "\n\n\n+" nil "move")
        (replace-match "\n\n"))


      ;; save buffer
      (save-buffer)
      (kill-buffer)
      )))


;; part 3: delte the html and md files
;; elisp 的 regex 非常有意思,需要对符号进行两次转义
(defun del-html-and-md (path)
  (setq subdirs (directory-files path t))

  (setq dirs-len (length subdirs))

  (setq dir-indices (number-sequence 2 (- dirs-len 1)))

  (dolist (diridx dir-indices)
    (progn
      (setq dir (elt subdirs diridx))
      (dolist (md-or-html (directory-files dir t ".html\\|.md\\|.ipython"))
        (progn
          (delete-file md-or-html)
          )
        )
      )
    )
  )

;; part 4: delte all "data:image" line
;; these lines are bad link reference produced by convert application
(defun del-dataimage-line (path)
  (setq subdirs (directory-files path t))

  (print subdirs)

  (setq dirs-len (length subdirs))

  ;; directory-files will include . directory and .. directory
  ;; exclude these two.
  (setq dir-indices (number-sequence 2 (- dirs-len 1)))

  (print dir-indices)

  (dolist (diridx dir-indices)
    (progn
      (setq dir (elt subdirs diridx))
      (dolist (orgfile (directory-files dir t "org"))
        (progn
          (print orgfile)
          (set-buffer (find-file-noselect orgfile))
          (while (search-forward "data:image" nil t)
            (print (buffer-name))
            (progn
              (beginning-of-line)
              (setq beginpos (point))
              (end-of-line)
              (setq endpos (point))
              (delete-region beginpos endpos)
              )
            )
          (save-buffer)
          )
        )
      )
    )
  )


;; part 5
;; extract python src from org file
(defun batch-tangle-org (path)
  (setq subdirs (directory-files path t))

  ;; (print subdirs)

  (setq dirs-len (length subdirs))

  ;; directory-files will include . directory and .. directory
  ;; exclude these two.
  (setq dir-indices (number-sequence 2 (- dirs-len 1)))

  ;; (print dir-indices)

  (dolist (diridx dir-indices)
    (progn
      (setq dir (elt subdirs diridx))
      (dolist (orgfile (directory-files dir t "org"))
        (progn
          (print orgfile)
          (setq targetfile (car (org-babel-tangle-file orgfile)))
          (rename-file targetfile
                       (concat (file-name-sans-extension targetfile) ".py"))
          )
        )
      )
    )
  )




;; combine part 1 and 2 and 3 and 4 and 5
;; ------------------------------------------------------------------------------
(defun yid-html-to-org (path)
  (dolist (subpath (directory-files path t)) (htmltoorg subpath))
  (dolist (subpath (directory-files path t)) (del-html-modify-exmp-block subpath))
  (del-html-and-md path)
  (del-dataimage-line path)
  (batch-tangle-org path)
  )

(setq labdir "/home/yiddi/git_repos/on_ml_wushanghong/ml/labs/")
(yid-html-to-org labdir)

(del-html-and-md labdir)

(del-dataimage-line labdir)

(batch-tangle-org compdir)

(setq compdir "/home/yiddi/git_repos/on_ml_wushanghong/ml/competitions/")
(yid-html-to-org compdir)
;; ------------------------------------------------------------------------------
