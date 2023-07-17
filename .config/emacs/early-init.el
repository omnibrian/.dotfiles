(setf
 default-frame-alist
 '((menu-bar-lines . 0)
   (tool-bar-lines . 0)
   (vertical-scroll-bars . nil)
   (horizontal-scroll-bars . nil)
   (child-frame-border-width . 2)))

(fset #'package--ensure-init-file #'ignore)
(fset #'x-apply-session-resources #'ignore)

(setf
 ;; inhibit-startup-screen    t
 package-enable-at-startup    nil
 gc-cons-threshold            most-positive-fixnum
 frame-inhibit-implied-resize t)
