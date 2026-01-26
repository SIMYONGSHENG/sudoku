;; ============================================================
;; 数独求解程序 - 使用回溯法和DFS两种搜索方法
;; ============================================================

;; ===== 1. 基础数据结构和函数 =====

;; 数独谜题示例 (0表示空单元格)
(defparameter *puzzle1*
  '((5 3 0 0 7 0 0 0 0)
    (6 0 0 1 9 5 0 0 0)
    (0 9 8 0 0 0 0 6 0)
    (8 0 0 0 6 0 0 0 3)
    (4 0 0 8 0 3 0 0 1)
    (7 0 0 0 2 0 0 0 6)
    (0 6 0 0 0 0 2 8 0)
    (0 0 0 4 1 9 0 0 5)
    (0 0 0 0 8 0 0 7 9)))

;; 复制棋盘
(defun copy-board (board)
  "创建棋盘的深拷贝"
  (mapcar #'copy-list board))

;; 获取单元格值
(defun get-cell (board row col)
  "获取(row, col)位置的值"
  (nth col (nth row board)))

;; 设置单元格值
(defun set-cell (board row col value)
  "设置(row, col)位置的值"
  (setf (nth col (nth row board)) value)
  board)

;; 检查行的有效性
(defun is-valid-row (board row num)
  "检查num是否已在该行出现"
  (not (member num (nth row board))))

;; 检查列的有效性
(defun is-valid-col (board col num)
  "检查num是否已在该列出现"
  (not (member num (mapcar (lambda (r) (nth col r)) board))))

;; 检查3x3子网格的有效性
(defun is-valid-box (board row col num)
  "检查num是否已在该3x3子网格出现"
  (let* ((box-row (* (floor (/ row 3)) 3))
         (box-col (* (floor (/ col 3)) 3))
         (box-nums nil))
    (dotimes (i 3)
      (dotimes (j 3)
        (push (get-cell board (+ box-row i) (+ box-col j)) box-nums)))
    (not (member num box-nums))))

;; 检查移动是否有效
(defun is-valid-move (board row col num)
  "检查在(row, col)位置放置num是否有效"
  (and (is-valid-row board row num)
       (is-valid-col board col num)
       (is-valid-box board row col num)))

;; 找到第一个空单元格
(defun find-empty-cell (board)
  "找到第一个空单元格,返回(row col)或nil"
  (dotimes (row 9)
    (dotimes (col 9)
      (when (= (get-cell board row col) 0)
        (return-from find-empty-cell (list row col)))))
  nil)

;; 检查棋盘是否已解决
(defun is-solved (board)
  "检查棋盘是否已完全填充"
  (not (find-empty-cell board)))

;; 打印棋盘
(defun print-board (board)
  "打印棋盘"
  (dotimes (row 9)
    (when (and (> row 0) (= (mod row 3) 0))
      (format t "------+-------+------~%"))
    (dotimes (col 9)
      (when (and (> col 0) (= (mod col 3) 0))
        (format t "| "))
      (let ((val (get-cell board row col)))
        (if (= val 0)
            (format t ". ")
            (format t "~d " val))))
    (format t "~%")))

;; ===== 2. 方法1: 回溯法 (Backtracking) =====

(defvar *backtrack-count* 0)

(defun solve-sudoku-backtrack (board &optional (count-nodes nil))
  "使用回溯法求解数独"
  (when count-nodes
    (setf *backtrack-count* 0))
  
  (labels ((backtrack (b)
             (when count-nodes
               (incf *backtrack-count*))
             
             ;; 基础情况:棋盘已解决
             (if (is-solved b)
                 (return-from backtrack t))
             
             ;; 找到第一个空单元格
             (let ((empty (find-empty-cell b)))
               (if (null empty)
                   (return-from backtrack t))
               
               (let ((row (first empty))
                     (col (second empty)))
                 ;; 尝试数字1-9
                 (dotimes (num 9)
                   (let ((digit (+ num 1)))
                     (when (is-valid-move b row col digit)
                       ;; 放置数字
                       (set-cell b row col digit)
                       
                       ;; 递归求解
                       (if (backtrack b)
                           (return-from backtrack t))
                       
                       ;; 回溯:移除数字
                       (set-cell b row col 0))))
                 
                 ;; 无法求解
                 (return-from backtrack nil)))))
    
    (let ((board-copy (copy-board board)))
      (if (backtrack board-copy)
          (values board-copy *backtrack-count*)
          (values nil *backtrack-count*)))))

;; ===== 3. 方法2: DFS (深度优先搜索) =====

(defvar *dfs-count* 0)

(defun get-candidates (board row col)
  "获取(row, col)位置的候选数字列表"
  (let ((candidates nil))
    (dotimes (num 9)
      (let ((digit (+ num 1)))
        (when (is-valid-move board row col digit)
          (push digit candidates))))
    (reverse candidates)))

(defun find-best-empty-cell (board)
  "找到候选数最少的空单元格(启发式)"
  (let ((best-cell nil)
        (min-candidates 10))
    (dotimes (row 9)
      (dotimes (col 9)
        (when (= (get-cell board row col) 0)
          (let ((candidates-count (length (get-candidates board row col))))
            (when (< candidates-count min-candidates)
              (setf best-cell (list row col))
              (setf min-candidates candidates-count))))))
    best-cell))

(defun solve-sudoku-dfs (board &optional (count-nodes nil))
  "使用DFS求解数独(带启发式选择)"
  (when count-nodes
    (setf *dfs-count* 0))
  
  (labels ((dfs (b)
             (when count-nodes
               (incf *dfs-count*))
             
             ;; 基础情况:棋盘已解决
             (if (is-solved b)
                 (return-from dfs t))
             
             ;; 找到候选数最少的空单元格
             (let ((empty (find-best-empty-cell b)))
               (if (null empty)
                   (return-from dfs t))
               
               (let ((row (first empty))
                     (col (second empty))
                     (candidates (get-candidates b (first empty) (second empty))))
                 
                 ;; 尝试每个候选数字
                 (dolist (digit candidates)
                   (set-cell b row col digit)
                   
                   ;; 递归求解
                   (if (dfs b)
                       (return-from dfs t))
                   
                   ;; 回溯
                   (set-cell b row col 0))
                 
                 ;; 无法求解
                 (return-from dfs nil)))))
    
    (let ((board-copy (copy-board board)))
      (if (dfs board-copy)
          (values board-copy *dfs-count*)
          (values nil *dfs-count*)))))

;; ===== 4. 验证和测试函数 =====

(defun verify-solution (board)
  "验证解答是否正确"
  (let ((valid t))
    ;; 检查所有行
    (dotimes (row 9)
      (let ((row-vals (nth row board)))
        (unless (= (length row-vals) 9)
          (setf valid nil))
        (unless (equal (sort (copy-list row-vals) #'<) '(1 2 3 4 5 6 7 8 9))
          (setf valid nil))))
    
    ;; 检查所有列
    (dotimes (col 9)
      (let ((col-vals (mapcar (lambda (r) (nth col r)) board)))
        (unless (equal (sort (copy-list col-vals) #'<) '(1 2 3 4 5 6 7 8 9))
          (setf valid nil))))
    
    ;; 检查所有3x3子网格
    (dotimes (box-row 3)
      (dotimes (box-col 3)
        (let ((box-vals nil))
          (dotimes (i 3)
            (dotimes (j 3)
              (push (get-cell board (+ (* box-row 3) i) (+ (* box-col 3) j)) box-vals)))
          (unless (equal (sort (copy-list box-vals) #'<) '(1 2 3 4 5 6 7 8 9))
            (setf valid nil)))))
    
    valid))

;; 测试程序
(defun test-sudoku ()
  "测试两种求解方法"
  (format t "~%========== 数独求解测试 ==========~%~%")
  
  (format t "原始谜题:~%")
  (print-board *puzzle1*)
  
  ;; 测试回溯法
  (format t "~%使用回溯法求解...~%")
  (let ((start-time (get-internal-run-time)))
    (multiple-value-bind (solution nodes) 
        (solve-sudoku-backtrack *puzzle1* t)
      (let ((end-time (get-internal-run-time))
            (time-ms (/ (- (get-internal-run-time) start-time) 1000.0)))
        (if solution
            (progn
              (format t "求解成功!~%")
              (format t "扩展节点数: ~d~%" nodes)
              (format t "耗时: ~2f ms~%" time-ms)
              (format t "验证: ")
              (if (verify-solution solution) 
                  (format t "✓ 正确~%")
                  (format t "✗ 错误~%"))
              (format t "~%解答:~%")
              (print-board solution))
            (format t "求解失败!~%")))))
  
  ;; 测试DFS
  (format t "~%使用DFS求解...~%")
  (let ((start-time (get-internal-run-time)))
    (multiple-value-bind (solution nodes)
        (solve-sudoku-dfs *puzzle1* t)
      (let ((time-ms (/ (- (get-internal-run-time) start-time) 1000.0)))
        (if solution
            (progn
              (format t "求解成功!~%")
              (format t "扩展节点数: ~d~%" nodes)
              (format t "耗时: ~2f ms~%" time-ms)
              (format t "验证: ")
              (if (verify-solution solution)
                  (format t "✓ 正确~%")
                  (format t "✗ 错误~%"))
              (format t "~%解答:~%")
              (print-board solution))
            (format t "求解失败!~%")))))
  
  (format t "~%========== 测试完成 ==========~%"))

;; 运行测试
(test-sudoku)