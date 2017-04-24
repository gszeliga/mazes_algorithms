(ns mazes.display.core-string)

(defn repeat-str
  "Create a string that repeats s n times."
  [s n]
  (apply str (repeat n s)))

(defn spaces
  "Create a string of n spaces."
  [n]
  (repeat-str \space n))

(defn center
  "Center s in padding to final size len"
  [s len]
  (let [slen (count s)
        lpad (int (/ (- len slen) 2))
        rpad (- len slen lpad)]
    (str (spaces lpad) s (spaces rpad))))

(defn with-spaces [_]
  "Renders a cell using plain spaces"
  (spaces 3))

(defn with-distances
  "Renders a cell showing its distance from a specific reference"
  [distances]
  #(center (str (get distances %)) 3))

(defn with-path
  "Renders a cell only when it's part of the shortest-path"
  [path]
  #(center (str (if (some (set [%]) path) "@" " ")) 3))
