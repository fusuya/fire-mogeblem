;;TODO 行動済み処理

(ql:quickload '(cl-charms bordeaux-threads alexandria defenum))

(setf sb-ext:*invoke-debugger-hook*
      (lambda (condition hook)
        (declare (ignore condition hook))
        ;; デバッガが呼ばれたら、単にプログラムを終了する
        ;; recklessly-p に t を指定して、後始末(標準出力ストリームのフラッシュ等)が行われないようにする
        ;;(sb-ext:quit :recklessly-p t)))
  ;;デバッガの文字ずれないようにする
       (charms/ll:endwin)))

(load "define.lisp")

(defmethod unit-jobdesc ((u unit))
  (aref *jobdescs* (unit-job u)))

(defmethod unit-weapondesc ((u unit))
  (aref *weapondescs* (unit-weapon u)))

;;ユニットのデータ作成
(defun make-unit-data (unit-num x y)
  (let* ((data (aref *units-data* unit-num))
         (name (aref data 0)) (job (aref data 1)) (hp (aref data 2))
         (str (aref data 3)) (skill (aref data 4))
         (w_lv (aref data 5)) (agi (aref data 6)) (luck (aref data 7))
         (def (aref data 8)) (move (aref data 9))
         (team (aref data 10)) (weapon (aref data 11)))
    (make-unit :name name :job job :hp hp :maxhp hp :str str :skill skill
         :w_lv w_lv :agi agi :luck luck :def def :move move :weapon weapon
         :x x :y y :unit-num unit-num :team team)))

;;地形データと全ユニットデータ作成
(defun make-cells-and-units (map map-no-chara)
  (let ((units-list nil) (cells (make-array (list *map-h* *map-w*)))
        (units nil))
    (loop for y from 0 below *map-h* do
      (loop for x from 0 below *map-w* do
       (let ((c (aref map (+ (* *map-w* y) x))))
          (if (numberp c)
              (setf (aref cells y x) c)
              (let ((unit-num (- (char-code (aref (symbol-name c) 0)) (char-code #\A))))
               (setf (aref cells y x) (aref map-no-chara (+ (* *map-w* y) x)))
               (push (make-unit-data unit-num x y) units-list))))))
    (setf units-list (sort units-list #'< :key #'(lambda (x) (unit-unit-num x))))
    (setf units (make-array (length units-list)
                 :initial-contents units-list))
    (values cells units)))


;;windowの枠
(defun draw-window-border (window
                           &optional
                             (ls #\|) (rs #\|) (ts #\-) (bs #\-)
                             (tl #\+) (tr #\+) (bl #\+) (br #\+))
  (apply #'charms/ll:wborder (charms::window-pointer window)
         (mapcar #'char-code (list ls rs ts bs tl tr bl br))))

(defun draw-window-box (window &optional (verch #\|) (horch #\-))
  (charms/ll:box (charms::window-pointer window) (char-code verch) (char-code horch)))

(defun start-color ()
  (when (eql (charms/ll:has-colors) charms/ll:false)
    (error "Your terminal does not support color."))
  (when (eql (charms/ll:can-change-color) charms/ll:false)
    (error "Your terminal does not support color."))
  (let ((ret-code (charms/ll:start-color)))
    (if (= ret-code 0)
     t
     (error "start-color error ~s." ret-code))))

;;カラーペアを作る
(defmacro define-color-pair ((name pair) foreground background)
  `(progn
     (start-color)
     (setf ,name (progn (charms/ll:init-pair ,pair ,foreground ,background)
                  (charms/ll:color-pair ,pair)))))


;;windowの背景色設定
(defun draw-window-background (window color-pair)
  (charms/ll:wbkgd (charms::window-pointer window) color-pair))

(defmacro with-colors ((window color-pair) &body body)
  (let ((winptr (gensym)))
    (alexandria:once-only (color-pair)
      `(let ((,winptr (charms::window-pointer ,window)))
        (charms/ll:wattron ,winptr ,color-pair)
        ,@body
        (charms/ll:wattroff ,winptr ,color-pair)))))

;;色作成
(defun init-color ()
  (define-color-pair (+white/blue+ 1) +white+ +blue+)
  (define-color-pair (+black/red+ 2) +black+ +red+)
  (define-color-pair (+black/white+ 3) +black+ +white+)
  (define-color-pair (+green/black+ 4) +green+ +black+)
  (define-color-pair (+dark_green/green+ 5) +dark_green+ +green+)
  (define-color-pair (+low-yama-f/low-yama-b+ 6) +low-yama-f+ +low-yama-b+)
  (define-color-pair (+high-yama-f/high-yama-b+ 7) +high-yama-f+ +high-yama-b+)
  (define-color-pair (+black/town-b+ 8) +black+ +town-b+)
  (define-color-pair (+black/fort-b+ 9) +black+ +fort-b+)
  (define-color-pair (+black/castle-b+ 10) +black+ +castle-b+)
  (define-color-pair (+black/green+ 11) +black+ +green+)
  (define-color-pair (+black/player-b+ 12) +black+ +player-b+)
  (define-color-pair (+black/p-move-b+ 13) +black+ +p-move-b+)
  (define-color-pair (+black/e-move-b+ 14) +black+ +e-move-b+)
  (define-color-pair (+black/atk-b+    15) +black+ +atk-b+))

;;疑似カーソル移動
(defun cursor-move (cursor x y window)
  (multiple-value-bind (width height)
      (charms:window-dimensions window)
    (if (/= x 0)
     (let* ((x1 (+ (cursor-x cursor) x))
            (x2 (+ (* x1 2) 1)))
        (cond
           ((>= x2 (1- width))
            (setf (cursor-x cursor) 0))
           ((<= x2 0)
            (setf (cursor-x cursor) (- *map-w* 1)))
           (t (setf (cursor-x cursor) x1))))
     (let ((y1 (+ (cursor-y cursor) y)))
        (cond
           ((>= y1 (- height 2))
            (setf (cursor-y cursor) 0))
           ((< y1 0)
            (setf (cursor-y cursor) (- *map-h* 1)))
           (t (setf (cursor-y cursor) y1)))))))

;;地形データ表示
(defun show-cell-data (cell window)
  (charms:write-string-at-point
   window
   (format nil "  地形  : ~a" (celldesc-name (aref *celldescs* cell)))
   1 1)
  (charms:write-string-at-point
   window
   (format nil "防御効果: ~2d%" (celldesc-def (aref *celldescs* cell)))
   1 2)
  (charms:write-string-at-point
   window
   (format nil "回復効果: ~a" (if (celldesc-heal (aref *celldescs* cell))
                           "あり" "なし"))
   1 3))

;; x y位置にユニットいたらユニットデータ返す
(defun get-unit (x y units)
  (loop for unit across units do
       (if (and (= (unit-x unit) x) (= (unit-y unit) y)
                (unit-alive? unit))
           (return-from get-unit unit))))

;;ユニットデータを表示
(defun show-unit-data (unit window)
  (let* ((weapon (unit-weapondesc unit))
         (w-name (weapondesc-name weapon))
         (w-dmg (weapondesc-damage weapon))
         (w-hit (weapondesc-hit weapon))
         (w-wei (weapondesc-weight weapon))
         (w-cri (weapondesc-critical weapon))
         (w-ranmin (weapondesc-rangemin weapon))
         (w-ranmax (weapondesc-rangemax weapon)))
   (charms:write-string-at-point
    window
    (format nil " 名前 : ~a ~a" (unit-name unit) (if (unit-act? unit) "(行動済み)" ""))
    1 1)
   (charms:write-string-at-point
    window
    (format nil "  HP  : ~2d/~2d" (unit-hp unit) (unit-maxhp unit))
    1 2)
   (charms:write-string-at-point
    window
    (format nil "  力  : ~2d" (unit-str unit))
    1 3)
   (charms:write-string-at-point
    window
    (format nil "  技  : ~2d" (unit-skill unit))
    1 4)
   (charms:write-string-at-point
    window
    (format nil "武器Lv: ~2d" (unit-w_lv unit))
    1 5)
   (charms:write-string-at-point
    window
    (format nil "素早さ: ~2d" (unit-agi unit))
    1 6)
   (charms:write-string-at-point
    window
    (format nil " 幸運 : ~2d" (unit-luck unit))
    1 7)
   (charms:write-string-at-point
    window
    (format nil "守備力: ~2d" (unit-def unit))
    1 8)
   (charms:write-string-at-point
    window
    (format nil "移動力: ~2d" (unit-move unit))
    1 9)
   (charms:write-string-at-point
    window
    (format nil " 武器 : ~a" w-name)
    1 10)
   (charms:write-string-at-point
    window
    (format nil "        威力:~2d 重量:~2d 命中:~2d~%         必殺:~2d レンジ:~d〜~d"
     w-dmg w-wei w-hit w-cri w-ranmin w-ranmax)
    1 11)))

;;地形の色取得
(defun get-cell-color (cell can-move)
  (cond
    ((and can-move (= can-move +ally+))
     +black/p-move-b+)
    ((and can-move (= can-move +enemy+))
     +black/e-move-b+)
    (t
     (cond
       ((= cell +cell_sea+)     +white/blue+)
       ((= cell +cell_plane+)   +black/green+)
       ((= cell +cell_forest+)  +dark_green/green+)
       ((= cell +cell_mt+)      +low-yama-f/low-yama-b+)
       ((= cell +cell_high_mt+) +high-yama-f/high-yama-b+)
       ((= cell +cell_town+)    +black/town-b+)
       ((= cell +cell_fort+)    +black/fort-b+)
       ((= cell +cell_castle+)  +black/castle-b+)))))

;;地形とユニット描画
(defun show-cell-unit (cells units window window2 unit-win cursor move-area atk-area)
  (loop for y from 0 below *map-h* do
       (loop for x from 0 below *map-w* do
        (let ((cell (aref cells y x)) (unit (get-unit x y units))
              (color +dark_green/green+) (aa nil))
           (if (and unit (unit-alive? unit))
               (progn
                 (if (= (unit-team unit) +ally+)
                     (setf color +black/player-b+)
                     (setf color +black/red+))
                 (setf aa (jobdesc-aa (unit-jobdesc unit))))
               (let ((can-move (aref move-area y x)))
                 (setf color (get-cell-color cell can-move))
                 (setf aa (celldesc-aa (aref *celldescs* cell)))))
           (when (aref atk-area y x)
             (setf color +black/atk-b+))
           (when (and (= (cursor-x cursor) x)
                      (= (cursor-y cursor) y))
               (setf color +black/white+)
               (show-cell-data cell window2)
               (when (and unit (unit-alive? unit))
                 (show-unit-data unit unit-win)))

           (with-colors (window color)
            (charms:write-string-at-point
               window aa (+ (* x 2) 1) (1+ y)))))))

;;敵移動中描画
;;地形とユニット描画
(defun show-map (cells units window)
  (loop for y from 0 below *map-h* do
    (loop for x from 0 below *map-w* do
       (let ((cell (aref cells y x)) (unit (get-unit x y units))
             (color +dark_green/green+) (aa nil))
         (if (and unit (unit-alive? unit))
             (let ((job-aa (jobdesc-aa (unit-jobdesc unit))))
                 (if (= (unit-team unit) +ally+)
                     (setf color +black/player-b+)
                     (setf color +black/red+))
                 (setf aa job-aa))
             (setf color (get-cell-color cell nil)
                   aa (celldesc-aa (aref *celldescs* cell))))
         (with-colors (window color)
             (charms:write-string-at-point
                window aa (+ (* x 2) 1) (1+ y)))))))

;;ユニットの移動可能範囲取得
(defun can-move-area (x y move movecost cells move-area team)
  (if (or (> 0 x) (> 0 y) (>= x *map-w*) (>= y *map-h*))
      nil
      (let* ((cell (aref cells y x))
             (cost (aref movecost cell)))
        (if (or (> cost move) (= cost -1))
            nil
            (progn
              (setf (aref move-area y x) team)
              (can-move-area (1+ x) y (- move cost) movecost cells move-area team)
              (can-move-area (1- x) y (- move cost) movecost cells move-area team)
              (can-move-area x (1+ y) (- move cost) movecost cells move-area team)
              (can-move-area x (1- y) (- move cost) movecost cells move-area team))))))

;;ユニットの移動可能範囲取得
(defun get-move-area (x y unit cells move-area)
  (let* ((move (unit-move unit))
         (team (unit-team unit))
         (movecost (jobdesc-movecost (unit-jobdesc unit))))
    (can-move-area x y move movecost cells move-area team)))

;;移動可能範囲の初期化
(defun init-move-area (move-area)
  (loop for y from 0 below *map-h* do
       (loop for x from 0 below *map-w* do
        (setf (aref move-area y x) nil))))

;;距離
(defun m-dist (x1 y1 x2 y2)
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

;;ユニットとユニットの距離
(defun unit-dist (unit1 unit2)
  (+ (abs (- (unit-x unit1) (unit-x unit2)))
     (abs (- (unit-y unit1) (unit-y unit2)))))

;;攻撃可能範囲取得
(defun can-atk-area? (x1 y1 x2 y2 r-min r-max atk-area i)
  (if (or (> 0 x2) (> 0 y2) (>= x2 *map-w*) (>= y2 *map-h*) (> 0 i))
      nil
      (let ((dist (m-dist x1 y1 x2 y2)))
        (if (>= r-max dist r-min)
            (setf (aref atk-area y2 x2) t))
        (can-atk-area? x1 y1 (1+ x2) y2 r-min r-max atk-area (1- i))
        (can-atk-area? x1 y1 (1- x2) y2 r-min r-max atk-area (1- i))
        (can-atk-area? x1 y1 x2 (1+ y2) r-min r-max atk-area (1- i))
        (can-atk-area? x1 y1 x2 (1- y2) r-min r-max atk-area (1- i)))))

;;攻撃可能範囲取得
(defun get-atk-area (unit atk-area)
  (let* ((weapon (unit-weapondesc unit))
         (r-min (weapondesc-rangemin weapon))
         (r-max (weapondesc-rangemax weapon))
         (x1 (unit-x unit))
         (y1 (unit-y unit)))
    (can-atk-area? x1 y1 x1 y1 r-min r-max atk-area r-max)))

;;攻撃可能範囲に敵がいるか判定
(defun can-atk? (atk-unit units atk-area)
  (loop for u across units
        do (if (and (aref atk-area (unit-y u) (unit-x u))
                    (unit-alive? u) (/= (unit-team atk-unit) (unit-team u)))
               (return-from can-atk? t))))

;;ユニット選択フェイズでzキー押したとき
(defun select-unit-p (units cells move-area cursor)
  (init-move-area move-area)
  (let ((unit (get-unit (cursor-x cursor) (cursor-y cursor) units))
        (s-mode nil) (select-unit nil))
    (if (and unit (unit-alive? unit) (null (unit-act? unit)))
        (let ((team (unit-team unit)))
          (setf select-unit unit)
          (if (= team +ally+)
              (setf s-mode +select_move+)
              (setf s-mode +select_unit+))
          (get-move-area (cursor-x cursor) (cursor-y cursor) unit cells move-area))
        (setf s-mode +select_unit+))
    (values select-unit s-mode)))

;;移動先選択フェイズでzキー押したとき
(defun select-move-p (select-unit units move-area atk-area s-mode cursor)
  (let ((unit (get-unit (cursor-x cursor) (cursor-y cursor) units)))
    (if (and (or (null unit) ;;ユニットがいない場所
                 (equal select-unit unit) ;;その場でとどまる
                 (null (unit-alive? unit))) ;;死亡しているユニットの場所
             (aref move-area (cursor-y cursor) (cursor-x cursor)))
        (progn
          (setf (unit-x select-unit) (cursor-x cursor)
                (unit-y select-unit) (cursor-y cursor))
          (init-move-area move-area)
          (get-atk-area select-unit atk-area)
          (let ((atk? (can-atk? select-unit units atk-area)))
            (if atk?
                (progn
                  (get-atk-area select-unit atk-area)
                  (setf s-mode +select_attack+))
                ;;攻撃できる敵がいないとき
                (progn (init-move-area atk-area)
                       (setf (unit-act? select-unit) t) ;;行動済み
                       (setf select-unit nil
                             s-mode +select_unit+))))))
    (values select-unit s-mode)))

;;攻撃メッセージ
(defun get-atk-msg (atk-type)
  (cond
    ((= atk-type +atk_normal+)
     "攻撃")
    ((= atk-type +atk_counter+)
     "反撃")
    ((= atk-type +atk_re+)
     "再攻撃")))

;;攻撃が命中してダメージを与える
(defun give-damage (atk-unit def-unit a-w-dmg a-w-cri atk-win)
  (let* ((a-str (unit-str atk-unit)) (a-luck (unit-luck atk-unit))
         (d-def (unit-def def-unit)) (a-skill (unit-skill atk-unit))
         (a-atk (+ a-str a-w-dmg))
         (hissatsu? (+ a-w-cri (floor (+ a-skill a-luck) 2)))
         (dmg (- a-atk d-def)) (msg-y 2))
    (when (> hissatsu? (random 100))
      (setf dmg (* dmg 3))
      (charms:write-string-at-point
        atk-win "必殺の一撃！" 1 (prog1 msg-y (incf msg-y))))
    (charms:write-string-at-point
      atk-win (format nil "~aに~dのダメージを与えた" (unit-name def-unit) dmg) 1 msg-y)
    (charms:get-char atk-win)
    (decf (unit-hp def-unit) dmg)))

;;命中しなかったメッセージ
(defun miss-msg (atk-win)
  (charms:write-string-at-point
    atk-win "ミス！！！！" 1 2)
  (charms:get-char atk-win))

;;HPが０になったときのメッセージ
(defun dead-msg (dead-unit alive-unit atk-win)
  (charms:write-string-at-point
    atk-win
    (if (= (unit-team dead-unit) +ally+)
        (format nil "~a は ~a に倒された。" (unit-name dead-unit) (unit-name alive-unit))
        (format nil "~a は ~a を倒した。" (unit-name alive-unit) (unit-name dead-unit)))
    1 4)
  (charms:get-char atk-win))

(defun celldesc-yx (cells y x)
  (aref *celldescs* (aref cells y x)))

;;攻撃処理
(defun attack! (atk-unit def-unit cells atk-type atk-win)
  (let* ((a-skill (unit-skill atk-unit))
         (a-agi (unit-agi atk-unit)) (d-agi (unit-agi def-unit))
         (a-weapon (unit-weapondesc atk-unit))
         (a-w-dmg (weapondesc-damage a-weapon)) (a-w-hit (weapondesc-hit a-weapon))
         (a-w-wei (weapondesc-weight a-weapon)) (a-w-cri (weapondesc-critical a-weapon))
         (d-weapon (unit-weapondesc def-unit))
         (d-w-wei (weapondesc-weight d-weapon))
         (d-r-min (weapondesc-rangemin d-weapon))
         (d-r-max (weapondesc-rangemax d-weapon))
         (dist (unit-dist atk-unit def-unit))
         (a-speed (- a-agi a-w-wei)) (d-speed (- d-agi d-w-wei))
         (cell (celldesc-yx cells (unit-y def-unit) (unit-x def-unit)))
         (cell-def (celldesc-def cell))
         (atk-hit (+ a-w-hit a-skill)) (def-dodge (+ cell-def (- d-agi d-w-wei)))
         (hit? (- atk-hit def-dodge)) (atk-msg (get-atk-msg atk-type)))
    (charms:clear-window atk-win)
    (charms:write-string-at-point
      atk-win (format nil "~aの~a" (unit-name atk-unit) atk-msg) 1 1)
    (draw-window-box atk-win)
    (charms:get-char atk-win)
    ;;命中したらダメージ
   (if (> hit? (random 100))
       (give-damage atk-unit def-unit a-w-dmg a-w-cri atk-win)
       (miss-msg atk-win))
   ;;反撃
   (when (and (> (unit-hp def-unit) 0) (= atk-type +atk_normal+)
              (>= d-r-max dist d-r-min))
     (attack! def-unit atk-unit cells +atk_counter+ atk-win))
   ;;再攻撃
   (when (and (> a-speed d-speed) (= atk-type +atk_normal+)
              (> (unit-hp def-unit) 0) (> (unit-hp atk-unit) 0))
     (attack! atk-unit def-unit cells +atk_re+ atk-win))
   ;;死亡判定
   (when (= atk-type +atk_normal+)
     (cond
       ((>= 0 (unit-hp def-unit))
        (setf (unit-alive? def-unit) nil)
        (dead-msg def-unit atk-unit atk-win))
       ((>= 0 (unit-hp atk-unit))
        (setf (unit-alive? atk-unit) nil)
        (dead-msg atk-unit def-unit atk-win))))

   (charms:refresh-window atk-win)))

;;攻撃先選択フェイズでzキー押したとき
(defun select-atk-p (atk-unit units cells atk-area s-mode cursor atk-win)
  (let ((def-unit (get-unit (cursor-x cursor) (cursor-y cursor) units)))
    (when (and def-unit (aref atk-area (cursor-y cursor) (cursor-x cursor))
             (/= (unit-team atk-unit) (unit-team def-unit)))
      (attack! atk-unit def-unit cells +atk_normal+ atk-win)
      (setf s-mode +select_unit+
            (unit-act? atk-unit) t ;;動いた
            atk-unit nil)
      (init-move-area atk-area))
    (values atk-unit s-mode)))

;;全員行動済みかチェック
(defun check-turn-end (units team)
  (loop for u across units
        do (when (and (= team (unit-team u))
                      (null (unit-act? u)))
             (return-from check-turn-end t))))

;;行動済み初期化
(defun init-act (units team)
  (loop for u across units
        when (= team (unit-team u))
        do
        (setf (unit-act? u) nil)))

;;一番近いキャラ
;;
(defun near-chara (unit units)
  (first
    (sort (remove-if (lambda (u) (or (null (unit-alive? u))
                                     (= (unit-team u) (unit-team unit))))
                     (coerce units 'list))
     #'<
     :key (lambda (u)
            (m-dist (unit-x unit) (unit-y unit) (unit-x u) (unit-y u))))))

;;敵が動いたら画面描画
(defun show-enemy-move (units cells window)
  (charms:clear-window window)
  (show-map cells units window)
  (draw-window-box window)
  (charms:write-string-at-point
    window "敵のターン" 28 0)
  (charms:refresh-window window)
  (sleep 0.2))

;;行くことのできる一歩先を探す
(defun search-1move (x y x1 y1 x2 y2 dist units cells move move-area movecost)
  (let ((canmove-l nil))
    (loop for i in '((0 1) (0 -1) (1 0) (-1 0))
          do (let ((d-x (+ x1 (car i)))
                   (d-y (+ y1 (cadr i))))
               (when (and (>= (1- *map-w*) d-x 0)
                          (>= (1- *map-h*) d-y 0))
                 (let* ((cell (aref cells d-y d-x))
                        (cost (aref movecost cell))
                        (unit2 (get-unit d-x d-y units))
                        (dist2 (m-dist d-x d-y x2 y2)))
                   (when (and (aref move-area d-y d-x)
                              (not (and (= x d-x) (= y d-y)))
                              (and unit2 (> (- move cost) 1)) ;;てすと
                              (>= cost 0) (> move cost))
                     (push (list (cons d-x d-y) (- dist dist2) cost) canmove-l))))))
    canmove-l))

;;てきとうむーぶ
;;x y 一歩前の位置　x1 y1が現在の位置 d-x d-yが一歩先候補位置
;; 一歩前と一歩先が同じにならないようにしてる
(defun tekito-move (x y unit units target r-min move-area movecost move cells window)
  (let* ((x1 (unit-x unit)) (y1 (unit-y unit))
         (x2 (unit-x target)) (y2 (unit-y target))
         (dist (m-dist x1 y1 x2 y2))) ;;ターゲットとの距離
    (when (and (> move 0) (> dist r-min)) ;;最小射程まで近づく
      (let ((canmove-l (search-1move x y x1 y1 x2 y2 dist units cells move move-area movecost))) ;;移動先候補リスト)
        (when canmove-l
          (let* ((dir (car (sort canmove-l #'> :key #'cadr)))
                 (cost (caddr dir)))
            (setf (unit-x unit) (caar dir)
                  (unit-y unit) (cdar dir))
            ;;敵が一歩動いたら画面描画
            (show-enemy-move units cells window)
            (tekito-move x1 y1 unit units target r-min move-area movecost (- move cost) cells window)))))))

;;指定した地形の(x y)座標を返す
(defun get-cell-pos (cell cells)
  (loop for y from 0 below *map-h* do
    (loop for x from 0 below *map-w* do
      (when (= (aref cells y x) cell)
        (return-from get-cell-pos (list x y))))))

;;城へ移動
(defun to-castle (unit units castle-pos move-area movecost move cells window)
  (let* ((x1 (unit-x unit)) (y1 (unit-y unit))
         (x2 (car castle-pos)) (y2 (cadr castle-pos))
         (dist (m-dist x1 y1 x2 y2)))
    (when (> move 0)
      (let ((canmove-l (search-1move 0 0 x1 y1 x2 y2 dist units cells move move-area movecost))) ;;移動先候補リスト)
        (when canmove-l
          (let* ((dir (car (sort canmove-l #'> :key #'cadr)))
                 (cost (caddr dir)))
            (setf (unit-x unit) (caar dir)
                  (unit-y unit) (cdar dir))
            ;;敵が一歩動いたら画面描画
            (show-enemy-move units cells window)
            (to-castle unit units castle-pos move-area movecost (- move cost) cells window)))))))
;;ボスは城へ移動
(defun move-to-castle (unit units cells move movecost move-area window)
  (let ((castle-pos (get-cell-pos +cell_castle+ cells)))
    (when (not (and (= (unit-x unit) (car castle-pos))
                    (= (unit-y unit) (cadr castle-pos))))
      (to-castle unit units castle-pos move-area movecost move cells window))))

;;敵の移動
(defun enemy-move (unit target r-min units cells move-area window)
  (init-move-area move-area)
  (get-move-area (unit-x unit) (unit-y unit) unit cells move-area)
  (let* ((move (unit-move unit))
         (movecost (jobdesc-movecost (unit-jobdesc unit))))
    (if (= (unit-team unit) +boss+)
        (move-to-castle unit units cells move movecost move-area window)
        (tekito-move 0 0 unit units target r-min move-area movecost move cells window))))

;;敵の攻撃
(defun enemy-attack (atk-unit def-unit cells atk-win)
  (attack! atk-unit def-unit cells +atk_normal+ atk-win)
  (charms:clear-window atk-win)
  (charms:refresh-window atk-win))

;;敵の行動 攻撃範囲に相手ユニットがいたら攻撃する
(defun enemy-act (units cells move-area atk-win window)
  (loop for u across units
        when (and (>= (unit-team u) +enemy+)
                  (unit-alive? u))
        do
        (let* ((weapon (unit-weapondesc u))
               (target (near-chara u units))
               (r-min (weapondesc-rangemin weapon))
               (r-max (weapondesc-rangemax weapon))
               (dist (unit-dist u target)))
          (if (>= r-max dist r-min) ;;攻撃範囲に相手がいる
              (enemy-attack u target cells atk-win)
              (progn ;;移動後に攻撃範囲に相手がいたら攻撃
                (enemy-move u target r-min units cells move-area window)
                (when (>= r-max (unit-dist u target) r-min)
                  (enemy-attack u target cells atk-win)))))))

;;回復地形効果
(defun cell-heal (units cells)
  (loop for u across units
    do
    (when (and (celldesc-heal (celldesc-yx cells (unit-y u) (unit-x u)))
               (> (unit-maxhp u) (unit-hp u)))
      (setf (unit-hp u)
            (+ (unit-hp u) (floor (unit-maxhp u) 10)))
      (when (> (unit-hp u) (unit-maxhp u))
        (setf (unit-hp u) (unit-maxhp u))))))

(defun hello-world ()
  (setf *random-state* (make-random-state t))
  (charms:with-curses ()
    (charms/ll:initscr)
    (charms:disable-echoing)
    (charms:enable-raw-input)
    (start-color)
    (init-color)
    (let ((cells nil) (units nil) (s-mode +select_unit+) (turn +p_turn+)
          (cursor (make-cursor :x 0 :y 0))
          (select-unit nil)
          (move-area (make-array (list *map-h* *map-w*) :initial-element nil))
          (atk-area  (make-array (list *map-h* *map-w*) :initial-element nil))
          (window (charms:make-window (+ 2 (* 2 *map-w*))
                     (+ 2 *map-h*) 0 0)))
             ;;特殊キーを使う
      (charms/ll:keypad (charms::window-pointer window) 1)
     (loop named hello-world
           with unit-win = (charms:make-window 34 14 0 (+ 2 *map-h*))
           with atk-win = (charms:make-window 36 8 0 (+ 2 *map-h*))
           with window2 = (charms:make-window 18 5 35 (+ 2 *map-h*))
       do (progn
            (charms:clear-window window)
            (charms:clear-window window2)
            (charms:clear-window unit-win)
         ;;(charms:clear-window atk-win)
         ;;地形とユニットセット
            (if (or (null cells) (null units))
                (setf (values cells units)
                      (make-cells-and-units *map1-chara* *map1-no-chara*)))

         ;;描画
            (show-cell-unit cells units window window2 unit-win cursor
                                   move-area atk-area)
         ;;ウィンドウの枠表示
            (draw-window-box window)
            (draw-window-box window2)
            (draw-window-box unit-win)

            (charms:write-string-at-point
             window
    ;;(format nil "x:~d y:~D" (cursor-x cursor) (cursor-y cursor)) 28 0)
             (format nil "turn:~d" turn) 28 0)
         ;;(charms:write-string-at-point window "マップ" 28 0)
            (charms:write-string-at-point window2 "地形データ" 3 0)
            (charms:write-string-at-point unit-win "ユニットデータ" 10 0)

            (charms:refresh-window window)
            (charms:refresh-window window2)
            (charms:refresh-window unit-win)

            (when (= turn +p_turn+)
              (let ((c (charms:get-char window)))
                (cond
                  ((eql c #\r)
                   (init-act units +ally+)
                   (setf turn +e_turn+))
                  ((eql c #\q)
                   (return-from hello-world))
                  ((eql c #\x)
                   (cond
                     ((= s-mode +select_unit+)
                      (init-move-area move-area))
                     ((= s-mode +select_move+)
                      (init-move-area move-area)
                      (setf s-mode +select_unit+
                            select-unit nil))
                     ((= s-mode +select_attack+)
                      (init-move-area atk-area)
                      (setf s-mode +select_unit+
                            (unit-act? select-unit) t
                            select-unit nil))))
                  ((eql c #\z)
                   (cond
                     ((= s-mode +select_unit+)
                      (setf (values select-unit s-mode)
                            (select-unit-p units cells move-area cursor)))
                     ((= s-mode +select_move+)
                      (setf (values select-unit s-mode)
                            (select-move-p select-unit units move-area atk-area s-mode cursor)))
                     ((= s-mode +select_attack+)
                      (setf (values select-unit s-mode)
                            (select-atk-p select-unit units cells atk-area s-mode cursor atk-win)))))
                  ((eql c (code-char charms/ll:key_up))
                   (cursor-move cursor 0 -1 window))
                  ((eql c (code-char charms/ll:key_down))
                   (cursor-move cursor 0 1 window))
                  ((eql c (code-char charms/ll:key_right))
                   (cursor-move cursor 1 0 window))
                  ((eql c (code-char charms/ll:key_left))
                   (cursor-move cursor -1 0 window)))))

            ;;ターンチェンジチェック
            (cond
              ((and (= turn +p_turn+)
                    (null (check-turn-end units +ally+)))
               (init-act units +ally+)
               (setf turn (- 1 turn))
               (cell-heal units cells))
              ((= turn +e_turn+)
               (enemy-act units cells move-area atk-win window)
               (setf turn (- 1 turn))
               (cell-heal units cells)))

            (charms:refresh-window window)
            (charms:refresh-window window2)
            (charms:refresh-window unit-win)
                   ;;(charms:refresh-window atk-win)

            (sleep 0.01))))))
