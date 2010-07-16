;; Set paths for various executables I use
(setq  gnuserv-program "F:\\tools\\bin\\gnuserv.exe"
       ispell-program-name "c:\\program files\\aspell\\bin\\aspell.exe"
       lua-default-application "f:\\tools\\wow-lua\\lua-wow.exe"
       epg-gpg-program "c:/Program Files/Gnu/GnuPG/gpg"
       plt-dir "c:/plt/"
       erc-dcc-get-default-directory "f:\\zz_download\\erc_dcc"
       gforth-dir "c:/progs/gforth"
       js-js-tmpdir (getenv "TMP")
       openssl-prg "f:/tools/msysgit/bin/openssl.exe")

;; This is Billy-land. No /usr/include for you!
(setq c-helper-global-search-list
      (list "c:/Program Files/Microsoft Visual Studio/VC98/include"))

;; Use plink, so that I get the agent goodness
(setq tramp-default-method "plink")
