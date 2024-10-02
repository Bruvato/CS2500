;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname problem1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

"Vietnam"
(overlay
 (star 30 "solid" "yellow")
 (rectangle 200 100 "solid" "red"))

"Chile"
(overlay/align "left" "top"
               (overlay
                (star 15 "solid" "white")
                (rectangle 50 50 "solid" "blue"))
               (rectangle 200 50 "solid" "white")
               (rectangle 200 100 "solid" "red"))

"Suriname"
(overlay
 (star 20 "solid" "yellow")
 (rectangle 200 30 "solid" "red")
 (rectangle 200 50 "solid" "white")
 (rectangle 200 100 "solid" "Sea Green"))

"Saint Lucia"
(overlay
 (overlay/align "middle" "bottom"
                (isosceles-triangle 40 80 "solid" "Gold")
                (isosceles-triangle 65 40 "solid" "black")
                (isosceles-triangle 75 40 "solid" "white"))
 (rectangle 200 100 "solid" "Light Sky Blue"))

"Turkey"
(overlay
 (overlay/offset
  (rotate -45 (star 20 "solid" "white"))
  -35 0
  (overlay/align/offset "right" "middle"
                 (circle 20 "solid" "red")
                 -2 0
                 (circle 25 "solid" "white")))
 (rectangle 200 100 "solid" "red"))