(when (require 'bbdb nil t)
  (bbdb-initialize 'message)
  (bbdb-insinuate-message)

  (setq bbdb-mua-pop-up t
        bbdb-complete-mail-allow-cycling t))

(setq message-alternative-emails (regexp-opt '("vedathallac@gmail.com"
                                               "vedat.hallac@gmail.com"
                                               "dys.wowace@gmail.com"
                                               "vedat@android.ciyiz.biz"
                                               "vedat@oyun.cuyuz.biz"
                                               "vedathallac@yandex.com"
                                               "vedat.hallac@pia-team.com")))
