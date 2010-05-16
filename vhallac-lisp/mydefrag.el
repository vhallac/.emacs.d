(defconst mydefrag-font-lock-keywords-1
  (list
   (list (regexp-opt '("VolumeSelect" "VolumeActions" "VolumeEnd" 
              "FileSelect" "FileActions" "FileEnd") 'words)
         '(1 font-lock-keyword-face))

   (list (regexp-opt '("Description" "MaxRunTime" "ExcludeVolumes" "ExcludeFiles"
                       "SetFileColor" "Message" "Language" "Title" "WindowSize"
                       "DiskmapFlip" "StatusBar" "ZoomLevel" "SetColor" "Slowdown"
                       "Pause" "WhenFinished" "OtherInstances" "RunScript"
                       "RunProgram" "BatteryPower" "SetScreenSaver"
                       "SetScreenPowerSaver" "FileMoveChunkSize" "Debug"
                       "SetStatisticsWindowText" "WriteLogfile" "AppendLogfile"
                       "ProcessPriority" "IgnoreWrapAroundFragmentation"
                       "RememberUnmovables") 'words)
         '(1 font-lock-function-name-face))

   (list (regexp-opt '("and" "or" "|" "||" "&" "&&" "not" "yes" "no") 'words)
         '(1 font-lock-keyword-face))) )

(defconst mydefrag-font-lock-keywords-2
  (append mydefrag-font-lock-keywords-1
          (list
           (list (regexp-opt '("All" "Mounted" "Writable" "Removable"
                               "Fixed" "Remote" "Cdrom" "Ramdisk"
                               "Name" "Label" "Size" "FragmentCount"
                               "FragmentSize" "CheckVolume" "CommandlineVolumes"
                               "NumberBetween" "FileSystemType") 'words)
                 '(1 font-lock-function-name-face))

           (list (regexp-opt '("NTFS" "FAT" "FAT12" "FAT16" "FAT32") 'words)
                 '(1 font-lock-constant-face)))) )

(defconst mydefrag-font-lock-keywords-3 mydefrag-font-lock-keywords-2)

(defconst mydefrag-font-lock-keywords mydefrag-font-lock-keywords-3
  "Default highlighting expressions for MyDefrag mode")

;;;###autoload
(define-derived-mode mydefrag-mode fundamental-mode "MyD"
  "Major mode for editing MyDefrag scripts.
"
  (set (make-local-variable 'font-lock-defaults)
       '(mydefrag-font-lock-keywords
         nil t
         ( (?/ . ". 124b")
           (?* . ". 23")
           (?# . "< b")
           (?\n . "> b")
           (?\' . "\"")
           (?\" . "\"")
         ) ) ) )

