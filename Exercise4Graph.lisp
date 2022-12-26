;;; Code for the exercise 4 of homework 4, GRAPH search.


;; Define the city structure, with the "visited" slot extra for the graph search
(defstruct city
  name
  neighbors
  h
  visited)


;; Define the global variables.
(defvar *all-cities-list* (list))

(defvar *all-cities-htable* (make-hash-table))

(defvar *nnv* 0)


;; Main function, just calls to the functions to create the map and do the search from the city indicated and the 
;; strategy indicated.
(defun main (city strategy)
  (setq input '((Arad 366
		     ((Zerind 75) (Timisoara 118) (Sibiu 140)))
	       (Bucharest 0
		     ((Giurgiu 90) (Pitesti 101) (Fagaras 211) (Urziceni 85)))
	       (Craiova 160
		     ((Dobreta 120) (Rimnicu_Vilcea 146) (Pitesti 138)))
	       (Dobreta 242
		     ((Mehadia 75) (Craiova 120)))
	       (Eforie 161
		     ((Hirsova 86)))
	       (Fagaras 176
		     ((Sibiu 99) (Bucharest 211)))
	       (Giurgiu 77
		     ((Bucharest 90)))
	       (Hirsova 151
		     ((Eforie 86) (Urziceni 98)))
	       (Iasi 226
		     ((Neamt 87) (Vaslui 92)))
	       (Lugoj 244
		     ((Timisoara 111) (Mehadia 70)))
	       (Mehadia 241
		     ((Lugoj 70) (Dobreta 75)))
	       (Neamt 234
		     ((Iasi 87)))
	       (Oradea 380
		     ((Zerind 71) (Sibiu 151)))
	       (Pitesti 100
		     ((Craiova 138) (Rimnicu_Vilcea 97) (Bucharest 101)))
	       (Rimnicu_Vilcea 193
		     ((Sibiu 80) (Pitesti 97) (Craiova 146)))
	       (Sibiu 253
		     ((Fagaras 99) (Rimnicu_Vilcea 80) (Arad 140) (Oradea 151)))
	       (Timisoara 329
		     ((Arad 118) (Lugoj 111)))
	       (Urziceni 80
		     ((Bucharest 85) (Hirsova 98) (Vaslui 142)))
	       (Vaslui  199
		     ((Iasi 92) (Urziceni 142)))
	       (Zerind 374
		     ((Arad 75) (Oradea 71)))))
  (insert-cities-hash input)
  (insert-cities-list input)
  (create-map-list input)
  (create-map-hash input)
  (time (make-graph-search (make-node :city (get-city-from-list city *all-cities-list*) :cost 0 :depth 0) strategy)))

;; Function that inserts the cities in the list
(defun insert-cities-list (input)
  (dolist (i input *all-cities-list*)
    (push (make-city :name (first i) :h (second i)) *all-cities-list*)))
    
;; Function that inserts the cities in the hash table   
(defun insert-cities-hash (input)
  (dolist (i input *all-cities-htable*)
    (setf (gethash (first i) *all-cities-htable*) (make-city :name (first i) :h (second i)))))

;; Auxiliar function for creating neighborhoods between cities using list
(defun create-neighbor-list (cityname1 cityname2 distance)
  (let ((city1 (find-if 
		#'(lambda(item) (equal (city-name item) cityname1))
		*all-cities-list*))
	(city2 (find-if
		#'(lambda(item) (equal (city-name item) cityname2))
		*all-cities-list*)))
    (pushnew (list city1 distance) (city-neighbors city2))))
 
 ;; Auxiliar function for creating neighborhoods between cities using hash table   
(defun create-neighbor-hash (cityname1 cityname2 distance)
  (let ((city1 (gethash cityname1 *all-cities-htable*))
	(city2 (gethash cityname2 *all-cities-htable*)))
    (pushnew (list city1 distance) (city-neighbors city2))))

;; Function that creates the neighbors of every city in the list
(defun create-map-list (input)
  (dolist (i input)
    (dolist (j (third i))
      (create-neighbor-list (first j) (first i) (second j)))))
      
;; Function that creates the neighbors of every city in the hash table      
(defun create-map-hash (input)
  (dolist (i input)
    (dolist (j (third i))
      (create-neighbor-hash (first j) (first i) (second j)))))
      
 
 ;; For all the following functions, that are asked to implement in the homework specification, the global variables used 
 ;; (*all-cities-list* and *all-cities-htable*) are passed as arguments, as it is requiered in the homework specification.
 
 ;; Function that returns a list with all the city names using the list    
(defun all-cities-from-list (*all-cities-list*)
  (let ((result nil))
    (dolist (i *all-cities-list*)
      (push (city-name i) result))
    result))
    

  ;; Function that returns a list with all the city names using the hash table 
(defun all-cities-from-htable (*all-cities-htable*)
  (let ((result nil))
    (loop for key being the hash-keys of *all-cities-htable*
	do (push (gethash key *all-cities-htable*) result))
    result ))
    
;; Function that returns the city structure given the name of the city, using the list.
(defun get-city-from-list (name *all-cities-list*)
  (find-if 
   #'(lambda(item) (equal (city-name item) name))
   *all-cities-list*))

;; Function that returns the city structure given the name of the city, using the hash table.
(defun get-city-from-htable (name *all-cities-htable*)
  (gethash name *all-cities-htable*))

;; Function that returns the neighbors of a city given its name, using the list.
(defun neighbors-using-list (name *all-cities-list*)
  (let ((city (get-city-from-list name *all-cities-list*)))
    (city-neighbors city)))

;; Function that returns the neighbors of a city given its name, using the hash table.
(defun neighbors-using-htable (name *all-cities-htable*)
  (let ((city (get-city-from-htable name *all-cities-htable*)))
    (city-neighbors city)))

;; Function that return all the neighbors of a city within a given distance.
(defun neighbors-within-d (my-city distance *all-cities-htable*)
  (let ((neighbors (neighbors-using-htable my-city *all-cities-htable*))
	(result nil))
    (dolist (i neighbors result)
      (if (<= (second i) distance)
	  (push i result)))))

;; Function that, given 2 city names, returns the distance between them if they are neighbors. Returns nil otherwise.
(defun neighbors-p (city-1 city-2 *all-cities-htable*)
  (let ((neighbors-1 (neighbors-using-htable city-1 *all-cities-htable*)))
    (second (find-if 
     #'(lambda(item) (equal (city-name (first item)) city-2))
     neighbors-1))))
    
    
    
    
;; Define node structure.
(defstruct node 
  city
  parent
  children
  cost
  depth)

;;Auxiliar function to expand a node. It generates the children and stores them in the children slot.
;; as this is graph search, it marks as visited the current node. And when generating the children, checks if the city has 
;; already beem visited.
(defun expand-node (node)
  (let* ((city (node-city node))
	 (neighbors (neighbors-using-htable (city-name city) *all-cities-htable*))
	 (cost (node-cost node))
	 (depth (node-depth node)))
    (setf *nnv* (+ *nnv* 1))
    (setf (city-visited city) T)
    (dolist (n neighbors)
      (unless (city-visited (first n))
	(push (make-node :city (first n) :parent node :cost (+ cost (neighbors-p (city-name city)  (city-name (first n)) *all-cities-htable*)) :depth (+ 1 depth)) (node-children node))))))

;; Auxiliar function to evaluate the node.
;; The greedy strategy just uses h.
;; A* uses h and the cost.
;; BFS, my uninformed choice, uses the depth as evaluation.
(defun evaluate-node (node strategy)
  (cond ((equal strategy 'BFS) (node-depth node))
	((equal strategy 'Greedy) (city-h (node-city node)))
	((equal strategy 'A*) (+ (node-cost node) (city-h (node-city node))))))


;; Auxiliar function that chooses from the fringe.
(defun choose-from-fringe (fringe strategy)
  (let ((node-min (first fringe)))
    (dolist (i fringe)
      (if (<= (evaluate-node i strategy) (evaluate-node node-min strategy))
	  (setf node-min i)))
	node-min))

;; Auxiliar function that, given a fringe and a node, generates a new fringe without that node.
;; This is used to get the node out of the fringe when required.
(defun new-fringe (fringe node)
  (let ((result (list)))
    (dolist (i fringe)
      (unless (equal i node)
	(push i result)))
    (reverse result)))
  
;; Auxiliar function that given the last node of the search goes up and builds the path.
(defun do-path (nodefinal)
  (do* ((node nodefinal (node-parent node))
	(path (list)))
	((null (node-parent node)) (push (city-name (node-city node)) path))
      (push (city-name (node-city node)) path)))
  
;; Main function of the graph search.
(defun make-graph-search (start strategy)
  (let ((fringe (list start))
	(current start))
    (setf *nnv* 1)
    (do ()
	((equal (city-name (node-city current)) 'Bucharest))
      (expand-node current)
      (dolist (i (node-children current))
	(push i fringe))
      (setf fringe (new-fringe fringe current))
      (setf current (choose-from-fringe fringe strategy)))
    (values (do-path current) *nnv* (node-cost current))
    ))
     
   
      
    
    
  
  

   



       
	
		


