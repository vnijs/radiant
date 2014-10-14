### Copying over the main code files
#    rsync --dry-run -avruz --exclude-from 'radiant-sync/exclude-from-sync2dropbox.txt' ~/Desktop/GitHub/radiant_dev/ ~/Dropbox/radiant/
# rsync -avruz --exclude-from 'radiant-sync/exclude-from-sync2dropbox.txt' ~/Desktop/GitHub/radiant_dev/ ~/Dropbox/radiant/

### Cleaning out stuff that is no longer needed
#      rsync --dry-run -avruz --exclude-from 'radiant-sync/exclude-from-sync2dropbox.txt' ~/Desktop/GitHub/radiant_dev/ ~/Dropbox/radiant/
#   rsync -avruz --exclude-from 'radiant-sync/exclude-from-sync2dropbox.txt' ~/Desktop/GitHub/radiant_dev/ ~/Dropbox/radiant/

### Cleaning out stuff that is no longer needed
#    rsync --dry-run -avruz --delete ~/Desktop/GitHub/radiant_dev/launchers/ ~/Dropbox/radiant/launchers/
#   rsync -avruz --delete ~/Desktop/GitHub/radiant_dev/launchers/ ~/Dropbox/radiant/launchers/
#   rsync --dry-run -avruz --delete ~/Desktop/GitHub/radiant_dev/inst/ ~/Dropbox/radiant/inst/
#  rsync -avruz --delete ~/Desktop/GitHub/radiant_dev/inst/ ~/Dropbox/radiant/inst/

# from http://superuser.com/questions/536561/rsync-using-regex-to-include-only-some-files
#   rsync --dry-run -avruzm -f'+ PACKAGE*' -f'+ */' -f'- *' ~/Desktop/GitHub/radiant_dev/radiant-miniCRAN/bin/ ~/Dropbox/radiant/radiant-miniCRAN/bin/
# rsync -avruzm -f'+ PACKAGE*' -f'+ */' -f'- *' ~/Desktop/GitHub/radiant_dev/radiant-miniCRAN/bin/ ~/Dropbox/radiant/radiant-miniCRAN/bin/

# only copy the files that do not exist to dropbox
#   rsync --dry-run -avrz --delete --ignore-existing ~/Desktop/GitHub/radiant_dev/radiant-miniCRAN/ ~/Dropbox/radiant/radiant-miniCRAN/
  rsync -avrz --delete --ignore-existing ~/Desktop/GitHub/radiant_dev/radiant-miniCRAN/ ~/Dropbox/radiant/radiant-miniCRAN/
