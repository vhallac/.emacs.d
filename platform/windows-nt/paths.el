;; Set paths for various executables I use
(setq  gnuserv-program "F:\\tools\\bin\\gnuserv.exe"
       ispell-program-name "c:\\program files\\aspell\\bin\\aspell.exe"
       lua-default-application "f:\\tools\\wow-lua\\lua-wow.exe"
       epg-gpg-program "c:/Program Files/Gnu/GnuPG/gpg2.exe"
       plt-dir "c:/plt/"
       erc-dcc-get-default-directory "f:\\zz_download\\erc_dcc"
       gforth-dir "c:/progs/gforth"
       js-js-tmpdir (getenv "TMP")
       openssl-prg "f:/tools/msysgit/bin/openssl.exe"
       magit-git-executable "f:/tools/msysgit/bin/git.exe"
       magit-gitk-executable "f:/tools/msysgit/bin/gitk"
)

;; This is Billy-land. No /usr/include for you!
(setq c-helper-global-search-list
      (list "c:/Program Files/Microsoft Visual Studio/VC98/include"))

;; Use plink, so that I get the agent goodness
(setq tramp-default-method "plink")

(add-to-list 'exec-path (convert-standard-filename "C:/progs/w3m"))
