(defparameter *map1-chara* ;;キャラ配置済み
  (make-array (* *map-h* *map-w*) :initial-contents
    '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 0 k k k 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0
      0 0 0 1 h 1 2 4 0 0 0 0 0 0 0 0 0 0 0 5 5 5 e 1 2 5 2 1 0 0
      0 0 1 1 1 1 1 4 4 0 0 0 0 0 0 6 1 1 1 1 1 1 1 1 f 1 5 1 1 0
      0 1 1 1 7 1 k 4 4 4 0 0 0 0 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 0
      0 1 k 1 1 1 2 4 4 4 4 4 4 1 k 1 1 1 1 1 0 0 0 b a g 1 1 1 0
      0 1 1 1 k k 1 2 4 4 4 2 1 1 1 2 1 1 1 0 0 0 0 1 6 1 1 1 1 0
      0 1 1 5 1 1 i 1 3 3 2 1 1 1 1 1 1 0 0 0 0 1 c d 1 1 1 1 0 0
      0 1 1 1 1 1 1 3 1 1 1 k 1 k 1 2 2 0 0 0 1 1 1 1 1 1 1 0 0 0
      0 0 1 1 1 k 1 1 6 1 1 1 1 1 2 2 0 0 0 2 1 j 1 1 2 1 0 0 0 0
      0 0 0 1 1 1 1 1 1 1 1 2 1 1 0 0 0 0 0 0 1 1 2 1 1 1 0 0 0 0
      0 0 0 0 1 5 5 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 2 2 0 0 0 0
      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

(defparameter *map1-enemy* ;;敵キャラのみ配置済み
  (make-array (* *map-h* *map-w*) :initial-contents
    '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 0 k k k 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0
      0 0 0 1 h 1 2 4 0 0 0 0 0 0 0 0 0 0 0 5 5 5 1 1 2 5 2 1 0 0
      0 0 1 1 1 1 1 4 4 0 0 0 0 0 0 6 1 1 1 1 1 1 1 1 1 7 5 1 1 0
      0 1 1 1 7 1 k 4 4 4 0 0 0 0 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 0
      0 1 k 1 1 1 2 4 4 4 4 4 4 1 k 1 1 1 1 1 0 0 0 1 1 1 1 1 1 0
      0 1 1 1 k k 1 2 4 4 4 2 1 1 1 2 1 1 1 0 0 0 0 1 6 1 1 1 1 0
      0 1 1 5 1 1 i 1 3 3 2 1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 1 1 0 0
      0 1 1 1 1 1 1 3 1 1 1 k 1 k 1 2 2 0 0 0 1 1 1 1 1 1 1 0 0 0
      0 0 1 1 1 k 1 1 6 1 1 1 1 1 2 2 0 0 0 2 1 j 1 1 2 1 0 0 0 0
      0 0 0 1 1 1 1 1 1 1 1 2 1 1 0 0 0 0 0 0 1 1 2 1 1 1 0 0 0 0
      0 0 0 0 1 5 5 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 2 2 0 0 0 0
      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

(defparameter *map1-no-unit* ;;キャラなし
  (make-array (* *map-h* *map-w*) :initial-contents
       '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
         0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0
         0 0 0 1 1 1 2 4 0 0 0 0 0 0 0 0 0 0 0 5 5 5 1 1 2 5 2 1 0 0
         0 0 1 1 1 1 1 4 4 0 0 0 0 0 0 6 1 1 1 1 1 1 1 1 1 7 5 1 1 0
         0 1 1 1 7 1 1 4 4 4 0 0 0 0 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 0
         0 1 1 1 1 1 2 4 4 4 4 4 4 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 0
         0 1 1 1 1 1 1 2 4 4 4 2 1 1 1 2 1 1 1 0 0 0 0 1 6 1 1 1 1 0
         0 1 1 5 1 1 1 1 3 3 2 1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 1 1 0 0
         0 1 1 1 1 1 1 3 1 1 1 1 1 1 1 2 2 0 0 0 1 1 1 1 1 1 1 0 0 0
         0 0 1 1 1 1 1 1 6 1 1 1 1 1 2 2 0 0 0 2 1 5 1 1 2 1 0 0 0 0
         0 0 0 1 1 1 1 1 1 1 1 2 1 1 0 0 0 0 0 0 1 1 2 1 1 1 0 0 0 0
         0 0 0 0 1 5 5 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 2 2 0 0 0 0
         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

(defparameter *map2-enemy* ;;敵キャラのみ配置済み
  (make-array (* *map-h* *map-w*) :initial-contents
	      '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 1 1 1 1 1 1 1 1 1 1 1
		1 5 1 1 2 2 2 1 1 1 4 1 4 1 1 1 1 1 5 3 3 L 3 3 3 2 1 1 1 1
		1 1 1 1 2 1 2 2 1 4 4 4 4 1 1 1 1 1 2 L 1 1 1 1 2 1 2 1 1 1
		1 1 1 1 1 1 1 1 1 4 4 3 3 1 1 1 1 1 1 1 1 1 1 1 1 1 3 1 1 0
		0 1 1 1 5 1 1 1 1 1 1 1 1 1 1 1 1 L 1 2 1 1 1 1 1 1 1 1 1 0
		0 1 1 1 1 1 2 1 2 2 2 2 2 1 2 1 1 1 1 i 1 1 2 K K 1 1 1 1 1
		0 1 1 1 1 1 1 2 2 2 2 2 1 1 1 2 1 1 K 1 i 1 1 1 1 1 M 1 1 0
		0 1 1 1 1 1 1 1 3 6 2 1 1 1 1 1 1 1 1 1 j 1 1 1 1 1 1 1 1 1
		0 1 1 1 1 1 1 3 1 1 3 3 2 2 1 2 2 1 L 1 1 1 1 3 L 3 1 1 1 1
		1 1 1 1 1 1 1 1 2 3 4 4 4 1 2 2 1 1 6 2 1 1 1 1 3 3 3 1 1 1
		1 1 1 1 1 1 1 3 3 4 4 2 2 2 2 1 1 1 1 4 4 1 2 1 4 4 4 3 3 1
		1 1 1 1 1 1 1 4 1 1 1 2 2 1 1 1 1 1 3 1 1 1 1 1 2 2 1 1 3 3
		1 1 1 1 1 1 1 4 1 1 1 1 1 1 1 1 1 1 3 1 1 1 1 1 1 1 1 1 1 1)))

(defparameter *map2-no-unit* ;;キャラなし
  (make-array (* *map-h* *map-w*) :initial-contents
	      '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 3 1 1 1 1 1 1 1 1 1 1 1
		1 5 1 1 2 2 2 1 1 1 4 1 4 1 1 1 1 1 5 3 3 3 3 3 3 2 1 1 1 1
		1 1 1 1 2 1 2 2 1 4 4 4 4 1 1 1 1 1 2 1 1 1 1 1 2 1 2 1 1 1
		1 1 1 1 1 1 1 1 1 4 4 3 3 1 1 1 1 1 1 1 1 1 1 1 1 1 3 1 1 0
		0 1 1 1 5 1 1 1 1 1 1 1 1 1 1 1 1 6 1 2 1 1 1 1 1 1 1 1 1 0
		0 1 1 1 1 1 2 1 2 2 2 2 2 1 2 1 1 1 1 1 1 1 2 1 1 1 1 1 1 1
		0 1 1 1 1 1 1 2 2 2 2 2 1 1 1 2 1 1 1 1 1 1 1 1 1 1 7 1 1 0
		0 1 1 1 1 1 1 1 3 6 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
		0 1 1 1 1 1 1 3 1 1 3 3 2 2 1 2 2 1 1 1 1 1 1 3 3 3 1 1 1 1
		1 1 1 1 1 1 1 1 2 3 4 4 4 1 2 2 1 1 6 2 1 1 1 1 3 3 3 1 1 1
		1 1 1 1 1 1 1 3 3 4 4 2 2 2 2 1 1 1 1 4 4 1 2 1 4 4 4 3 3 1
		1 1 1 1 1 1 1 4 1 1 1 2 2 1 1 1 1 1 3 1 1 1 1 1 2 2 1 1 3 3
		1 1 1 1 1 1 1 4 1 1 1 1 1 1 1 1 1 1 3 1 1 1 1 1 1 1 1 1 1 1)))

(defparameter *map3-no-unit* ;;敵キャラのみ配置済み
  (make-array (* *map-h* *map-w*) :initial-contents
	      '(4 4 4 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 4 4 4 4 4 4 4 4 4
		4 4 4 3 2 2 2 1 1 1 1 1 1 1 1 5 1 1 1 1 1 1 3 3 3 3 4 4 4 4
		4 4 3 3 2 1 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 2 3 3 3
		3 3 3 2 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 3 3 1 1 1
		1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 2 6 3 1 1 1
		1 1 1 1 1 1 2 1 2 2 2 2 2 1 2 1 1 1 1 2 1 1 2 1 3 2 1 1 1 1
		5 1 1 1 1 1 1 1 2 1 2 2 1 1 6 2 1 1 2 1 1 1 1 3 3 1 1 1 1 2
		1 1 1 1 1 1 1 1 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
		1 1 1 1 1 1 1 3 1 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 2 1 1 1 1 1
		1 1 4 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 2 1 2 1 1 1
		1 1 4 4 1 1 1 1 1 1 1 1 2 2 2 1 1 1 1 1 1 1 2 1 1 1 1 7 1 0
		1 1 1 1 4 4 4 3 1 1 1 2 2 1 1 1 1 1 2 1 1 1 1 1 2 2 1 1 0 0
		1 1 1 1 1 1 4 4 1 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0)))

(defparameter *map4-no-unit* ;;敵キャラのみ配置済み
  (make-array (* *map-h* *map-w*) :initial-contents
	      '(4 4 4 1 1 1 1 1 1 4 4 4 2 2 2 4 4 4 2 2 2 4 4 4 4 4 4 4 4 4
		4 4 4 3 2 2 2 1 1 4 4 4 1 2 1 4 4 4 1 1 1 4 4 4 3 3 4 4 4 4
		4 4 4 3 6 1 2 2 1 1 1 1 1 1 1 4 4 4 1 1 1 1 1 1 2 1 2 3 3 3
		3 3 3 2 1 1 1 1 1 1 1 1 1 1 1 4 4 4 1 1 1 1 1 1 1 1 3 1 1 1
		1 1 1 1 1 1 1 1 1 4 4 4 1 1 1 4 4 4 1 1 1 1 1 1 2 1 1 1 1 1
		1 1 1 1 1 1 2 1 2 4 4 4 2 1 2 4 4 4 1 2 1 4 4 4 3 2 1 1 1 1
		1 1 1 1 1 1 1 1 2 4 4 4 1 1 6 4 4 4 6 1 1 4 4 4 3 1 1 1 5 2
		1 1 1 1 1 1 1 1 3 4 4 4 1 1 1 4 4 4 1 1 1 4 4 4 1 1 1 1 1 1
		1 1 1 7 1 1 1 3 1 4 4 4 1 1 1 1 1 1 1 1 1 4 4 4 2 1 1 1 1 1
		1 1 1 1 1 1 1 1 1 4 4 4 1 1 1 1 1 1 1 1 1 4 4 4 2 1 2 1 1 1
		1 1 1 1 1 1 1 1 1 3 3 3 2 2 2 1 6 1 1 1 1 4 4 4 1 1 1 7 1 0
		1 1 1 1 4 4 3 3 1 4 4 4 2 1 1 4 4 4 1 5 1 4 4 4 2 2 1 1 0 0
		1 1 1 1 1 1 4 4 1 4 4 4 3 3 3 4 4 4 3 3 3 4 4 4 1 1 1 1 0 0)))

(defparameter *map5-no-unit* ;;敵キャラのみ配置済み
  (make-array (* *map-h* *map-w*) :initial-contents
	      '(4 4 4 1 1 1 1 1 1 4 4 4 2 2 2 1 1 1 2 2 2 4 4 4 4 4 4 4 4 4
		4 4 4 3 2 2 2 1 1 3 3 3 1 2 1 1 1 1 1 1 1 4 4 4 3 3 3 3 3 3
		1 1 1 3 6 1 2 2 1 1 1 1 1 1 1 1 6 1 1 5 1 1 1 1 2 1 2 3 3 3
		2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 1 1 1
		1 1 1 1 1 1 7 1 1 2 1 2 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1
		1 1 3 1 1 1 1 1 2 1 1 2 2 1 2 1 1 1 1 2 1 1 1 1 1 2 1 1 1 1
		1 1 3 1 1 1 1 1 2 3 3 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 1 1 2
		1 1 1 1 1 1 1 1 3 3 3 1 1 1 1 1 1 1 3 3 1 1 1 1 1 1 1 1 1 1
		1 1 5 1 1 1 1 3 1 1 1 1 6 1 1 1 1 1 1 1 1 2 2 1 2 1 1 1 1 1
		1 1 5 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 2 1 1 1
		1 1 1 1 1 1 1 1 1 3 3 1 2 2 2 1 6 1 1 1 1 1 1 1 1 1 1 1 1 1
		1 1 1 1 2 2 3 3 1 1 1 1 2 1 1 1 1 1 1 5 1 1 1 1 2 2 1 1 1 1
		1 1 1 1 1 1 4 4 1 4 4 4 3 3 3 2 2 2 3 3 3 4 4 4 1 1 1 1 1 1)))

;;すべてのマップリスト
(defparameter *all-enemy-map*
  (list nil *map1-enemy* *map2-enemy*))

(defparameter *all-no-unit-map*
  (list nil *map1-no-unit* *map2-no-unit*))

;;ステージごとの初期位置(真ん中) (y x)
(defparameter *stage-init-pos*
  '((0 0) (6 24) (10 2)))