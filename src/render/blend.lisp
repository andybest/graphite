(in-package :render)

(defun blend-pixel (mode src dest)
  "Blends source RGBA pixel onto a destination RGB pixel given a particular blend mode"
  (ecase mode
    ; Just return the RGB values of the source
    (:no-blend (subseq src 0 3))

    ; Standard alpha blending
    (:blend (utils:with-aref (r1 b1 g1) dest
              (utils:with-aref (r2 g2 b2 a1) src
                (let ((alpha (+ a1 1))
                      (invalpha (- 256 a1)))
                  (vector
                   #I(((alpha * r2 + invalpha * r1) >> 8) & #xff)
                   #I(((alpha * g2 + invalpha * g1) >> 8) & #xff)
                   #I(((alpha * b2 + invalpha * b1) >> 8) & #xff))))))))
