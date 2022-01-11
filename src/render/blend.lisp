(in-package #:graphite.renderer)

(named-readtables:in-readtable cmu-infix:syntax)

(defun blend-pixel (mode src dest)
  "Blends the source RGBA pixel onto a destination RGB pixel given a particular blend mode.
   Expects a vec4 for the source and a vec3 for the destination.
   Values are left unclamped (can extend past the range of 0.0 to 1.0) so exposure can be
   adjusted later."
  (ecase mode
    (:no-blend (subseq src 0 3))

    ; Standard alpha blending
    (:blend (let* ((src-rgb (v:swizzle src :xyz))
                   (alpha (v:w src))
                   (alpha-inv (- 1 alpha)))
              (v3:+ (v3:*s src-rgb alpha)
                    (v3:*s dest alpha-inv))))

    (:add (v3:+ (v:swizzle src :xyz) dest))))


(defun blend-pixel-byte (mode src dest)
  "Blends source RGBA pixel onto a destination RGB pixel given a particular blend mode"
  (ecase mode
    ; Just return the RGB values of the source
    (:no-blend (subseq src 0 3))

    ; Standard alpha blending
    (:blend (with-aref (r1 b1 g1) dest
              (with-aref (r2 g2 b2 a1) src
                (let ((alpha (+ a1 1))
                      (invalpha (- 256 a1)))
                  (vector
                   #I(((alpha * r2 + invalpha * r1) >> 8) & #xff)
                   #I(((alpha * g2 + invalpha * g1) >> 8) & #xff)
                   #I(((alpha * b2 + invalpha * b1) >> 8) & #xff))))))

    ; Additive blending
    (:add (with-aref (r1 b1 g1) dest
            (with-aref (r2 b2 g2 a2) src
              (vector
               (min 255 (+ r1 r2))
               (min 255 (+ g1 g2))
               (min 255 (+ b1 b2))))))))
