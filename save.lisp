;;TODO 
;;謎の文字列
(defun rand-string ()
  (let ((str))
    (dotimes (i 16)
      (case (random 2)
	(0 (push (code-char (+ 97 (random 26))) str))
	(1 (push (code-char (+ 48 (random 10))) str))))
    (coerce str 'string)))



;;(name job hp maxhp str skill w_lv agi luck def give-exp move weapon rank lv)
(defun make-units-data-list (units)
  (loop for u across units
     collect (list (unit-name u) (unit-job u) (unit-hp u) (unit-maxhp u) (unit-str u)
		   (unit-skill u) (unit-w_lv u) (unit-agi u) (unit-luck u) (unit-def u)
		   (unit-give_exp u) (unit-move u) (unit-team u) (unit-weapon u) (unit-rank u)
		   (unit-lv u) (unit-lvup u) (unit-item u) (unit-exp u))))

;;謎のコード
(defun my-getstr (window)
  (cffi:with-foreign-string
      (str "")
    (charms/ll:wgetstr (charms::window-pointer window) str)
    (cffi:foreign-string-to-lisp str)))

;;セーブする
(defun save-suru (game)
  ;; (charms:enable-echoing)
  ;; (charms/ll:curs-set 1)
  (let* ((stage (game-stage game))
	 (path "./save/")
	 (str1 (rand-string))
	 (str (concatenate 'string path str1))
	 (units-data (make-units-data-list (game-player_units game)))
	 (lst units-data)
	 (window (charms:make-window 60 8 5 5)))
    ;; (charms:write-string-at-point
    ;;  window
    ;;  "セーブファイルに名前をつけてください(全角８文字まで)" 1 1)
    ;; (charms:move-cursor window 1 2)
    ;; (setf str1 (my-getstr window))
    ;; (when (equal "" str1)
    ;;   (setf str1 (rand-string)))
    ;; (when (>= (length str1) 9) ;;９文字以上だと８文字に強制で短くする
    ;;   (setf str1 (subseq str1 0 8)))
    ;;(setf str (concatenate 'string path str1))
    (with-open-file (out str :direction :output
			 :if-exists :supersede)
      (format out "(setf *load-units-data* '~s)~%" lst)
      (format out "(setf *load-stage* ~d)~%" stage)
      (format out "(setf *load-money* ~d)" (game-money game)))
    (charms:write-string-at-point
     window
     "セーブしました" 1 4)
    (charms:write-string-at-point
     window
     (format nil "復活の呪文は~% ~a~% です" str1) 1 5)
    (refresh-windows window)
    ;; (charms:disable-echoing)
    ;; (charms/ll:curs-set 0)
    (charms:get-char window)
    (erase-window window)
    (charms:destroy-window window)))





(defun load-file (str)
  (when str
    (handler-case
	(load (concatenate 'string "./save/" str))
      (file-error ()
	nil))))


;;ロードする
(defun load-suru (game)
  (let ((len (length *load-units-data*)))
    (setf (game-player_units game)
	  (make-array len :initial-contents
		      (loop for u in *load-units-data*
			 collect (apply #'make-unit
					(mapcan #'list
						'(:name :job :hp :maxhp :str :skill
						  :w_lv :agi :luck :def :give_exp :move :team :weapon :rank :lv :lvup :item :exp)
						u))))
	  (game-stage game) *load-stage*
	  (game-money game) *load-money*)))

;;ロードしたいファイルを選択する
(defun select-load-file (cursor files num select-win)
  (clear-windows select-win)
  (loop for name in files
     for y from 1 
     do (let ((color +white/black+))
	  (when (= y (1+ cursor))
	    (setf color +black/white+))
	  (with-colors (select-win color)
	    (charms:write-string-at-point
	     select-win
	     (format nil "~A" name)
	     1 y))))
  (draw-windows-box select-win)
  (refresh-windows select-win)
  (let ((c (charms:get-char select-win)))
     (cond
       ((eql c #\z) ;;決定
	(nth cursor files))
       ((eql c #\q) ;;ゲーム終了
	nil)
       ((eql c (code-char charms/ll:key_up))
        (if (> 0 (1- cursor))
	    (select-load-file (1- num) files num select-win)
	    (select-load-file (1- cursor) files num select-win)))
       ((eql c (code-char charms/ll:key_down))
	(if (>= (1+ cursor) num)
	    (select-load-file 0 files num select-win)
	    (select-load-file (1+ cursor) files num select-win)))
       (t
	(select-load-file cursor files num select-win)))))

;;復活の呪文入力
(defun get-loadstr (game)
  (charms:enable-echoing)
  (charms/ll:curs-set 1)
  (let ((window (charms:make-window 40 5 0 0))
	(files (map 'list #'pathname-name  (directory "./save/*")))) ;;セーブファイル名リスト
    (cond
      (files
	(charms:write-string-at-point
	 window
	 "復活の呪文を入力してください" 1 1)
	(charms:move-cursor window 1 2)
	(refresh-windows window)
	(let* ((str (my-getstr window)));;(num (length files))
	       ;;(select-win (charms:make-window 40 (+ num 2) 0 7)))
	  ;;(charms/ll:keypad (charms::window-pointer select-win) 1) ;;カーソルキー
	  (if (and (find str files :test #'equal) ;;セーブフォルダにあるファイルか
		   (load-file str));;(select-load-file 0 files num select-win))
	      (progn (charms:write-string-at-point
		      window
		      "ロードしました" 1 3)
		     (load-suru game)
		     ;;(delete-file loadstr) ;;ろーどしたファイル削除
		     (setf *game-opening* nil
			   *load-game* t))
	      (charms:write-string-at-point
	       window
	       "復活の呪文が間違ってます！！" 1 3))))
	  ;;(erase-window select-win)
	  ;;(destroy-windows select-win)))
      (t
       (charms:write-string-at-point
	 window
	 "セーブファイルがありません" 1 1)))
    (refresh-windows window)
    (charms:disable-echoing)
    (charms/ll:curs-set 0)
    (charms:get-char window)
    (erase-window window)
    (destroy-windows window)))

