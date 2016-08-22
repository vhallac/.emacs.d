(eval-when-compile
  (require 'projectile))

(projectile-register-project-type 'ant '("build.xml") "ant" "ant test")
(setq projectile-project-root-files-functions '(projectile-root-top-down
                                                projectile-root-bottom-up
                                                projectile-root-top-down-recurring)
      projectile-project-root-files (cons "build.xml" projectile-project-root-files))
