### Copying over the main code files
# rsync --dry-run -avruz --exclude-from 'radiant-sync/exclude-from-sync2dropbox.txt' ~/Desktop/GitHub/radiant_dev/ ~/Dropbox/radiant/
# rsync -avruz --exclude-from 'radiant-sync/exclude-from-sync2dropbox.txt' ~/Desktop/GitHub/radiant_dev/ ~/Dropbox/radiant/

### Cleaning out stuff that is no longer needed
# rsync --dry-run -avruz --exclude-from 'radiant-sync/exclude-from-sync2dropbox.txt' ~/Desktop/GitHub/radiant_dev/ ~/Dropbox/radiant/
# rsync -avruz --exclude-from 'radiant-sync/exclude-from-sync2dropbox.txt' ~/Desktop/GitHub/radiant_dev/ ~/Dropbox/radiant/

### Cleaning out stuff that is no longer needed
# rsync --dry-run -avruz --delete ~/Desktop/GitHub/radiant_dev/launchers/ ~/Dropbox/radiant/launchers/
# rsync -avruz --delete ~/Desktop/GitHub/radiant_dev/launchers/ ~/Dropbox/radiant/launchers/
# rsync --dry-run -avruz --delete ~/Desktop/GitHub/radiant_dev/inst/ ~/Dropbox/radiant/inst/
# rsync -avruz --delete ~/Desktop/GitHub/radiant_dev/inst/ ~/Dropbox/radiant/inst/


# diff -x "*.gz" -Nr ~/Desktop/GitHub/radiant_dev/miniCRAN/bin/ ~/Dropbox/radiant/miniCRAN/bin/
# diff -Nr ~/Desktop/GitHub/radiant_dev/miniCRAN/bin/windows/contrib/3.1/PACKAGES ~/Dropbox/radiant/miniCRAN/bin/windows/contrib/3.1/PACKAGES
# diff -Nr ~/Desktop/GitHub/radiant_dev/miniCRAN/bin/macosx/contrib/3.1/PACKAGES ~/Dropbox/radiant/miniCRAN/bin/macosx/contrib/3.1/PACKAGES
# diff -x ".DS_Store" -Naur ~/Desktop/GitHub/radiant_dev/launchers/ ~/Dropbox/radiant/launchers/

# from http://superuser.com/questions/536561/rsync-using-regex-to-include-only-some-files
# rsync -navruzm -f'+ PACKAGE*' -f'+ */' -f'- *' ~/Desktop/GitHub/radiant_dev/miniCRAN/ ~/Dropbox/radiant/miniCRAN/
# rsync -avruzm -f'+ PACKAGE*' -f'+ */' -f'- *' ~/Desktop/GitHub/radiant_dev/miniCRAN/ ~/Dropbox/radiant/miniCRAN/
# only copy the files that do not exist to dropbox
# rsync -navrz --delete --ignore-existing ~/Desktop/GitHub/radiant_dev/miniCRAN/ ~/Dropbox/radiant/miniCRAN/

# rsync -avrz --delete --ignore-existing ~/Desktop/GitHub/radiant_dev/miniCRAN/ ~/Dropbox/radiant/miniCRAN/

# rsync --dry-run -avruz --delete ~/Desktop/GitHub/radiant_dev/dev/base/ ~/Dropbox/radiant/dev/base/
# rsync --dry-run -avruz --delete ~/Desktop/GitHub/radiant_dev/dev/base/ ~/Dropbox/radiant/dev/base/

# rsync -avruz --delete ~/Desktop/GitHub/radiant_dev/dev/base/ ~/Dropbox/radiant/dev/base/

# syncing the dev branch
# rsync --dry-run -avruz --delete ~/Desktop/GitHub/radiant_dev/dev/base/ ~/Dropbox/radiant/dev/base/
# rsync --dry-run -avruz --delete ~/Desktop/GitHub/radiant_dev/dev/quant/ ~/Dropbox/radiant/dev/quant/
# rsync --dry-run -avruz --delete ~/Desktop/GitHub/radiant_dev/dev/marketing/ ~/Dropbox/radiant/dev/marketing/

# rsync -avruz --delete ~/Desktop/GitHub/radiant_dev/dev/base/ ~/Dropbox/radiant/dev/base/
# rsync -avruz --delete ~/Desktop/GitHub/radiant_dev/dev/quant/ ~/Dropbox/radiant/dev/quant/
# rsync -avruz --delete ~/Desktop/GitHub/radiant_dev/dev/marketing/ ~/Dropbox/radiant/dev/marketing/

# diff -x ".DS_Store" -Naur ~/Dropbox/radiant/inst/ ~/Desktop/GitHub/radiant_dev/inst/
# diff -x ".DS_Store" -Naur ~/Dropbox/radiant/inst/ ~/Desktop/GitHub/radiant_dev/dev/
# diff -x ".DS_Store" -x "*.rda" -Naur ~/Dropbox/radiant/inst/base/ ~/Desktop/GitHub/radiant_dev/dev/base/

### If I push and commit this change and then sync with dropbox, if I need to revert the commit then
### I can just revert and resync

# after this one you can see all the changes nicely in the commit screen!
# rsync --dry-run -av --delete ~/Desktop/GitHub/radiant_dev/dev/ ~/Desktop/GitHub/radiant_dev/inst/
# rsync --dry-run -av --delete ~/Desktop/GitHub/radiant_dev/dev/  ~/Dropbox/radiant/dev/
# rsync --dry-run -av --delete ~/Desktop/GitHub/radiant_dev/inst/  ~/Dropbox/radiant/inst/
# rsync -av --delete ~/Desktop/GitHub/radiant_dev/dev/  ~/Dropbox/radiant/dev/

# rsync -av --delete ~/Desktop/GitHub/radiant_dev/dev/base/tools/help/ ~/Desktop/GitHub/radiant_dev/inst/base/tools/help/
# rsync --dry-run -av --delete ~/Desktop/GitHub/radiant_dev/dev/base/tools/ ~/Desktop/GitHub/radiant_dev/inst/base/tools/

# rsync --dry-run -av --delete ~/Desktop/GitHub/radiant_dev/dev/ ~/Desktop/GitHub/radiant_dev/inst/
# rsync -av --delete ~/Desktop/GitHub/radiant_dev/dev/ ~/Desktop/GitHub/radiant_dev/inst/
# rsync -av ~/Desktop/GitHub/radiant_dev/dev/  ~/Dropbox/radiant/dev/
# rsync -av ~/Desktop/GitHub/radiant_dev/inst/  ~/Dropbox/radiant/inst/

#  rsync -av ~/Desktop/GitHub/radiant_dev/dev/ ~/Desktop/GitHub/radiant_dev/inst/
# rsync -av ~/Desktop/GitHub/radiant_dev/inst/  ~/Dropbox/radiant/inst/

# rsync -av ~/Desktop/GitHub/radiant_dev/dev_branch/ ~/Desktop/GitHub/radiant_dev/dev/
# rsync -av ~/Desktop/GitHub/radiant_dev/dev/  ~/Dropbox/radiant/dev/

# rsync -nav ~/Desktop/GitHub/radiant_dev/dev_branch/ ~/Desktop/GitHub/radiant_dev/dev/
# rsync -av ~/Desktop/GitHub/radiant_dev/dev/  ~/Dropbox/radiant/dev/

# rsync -av ~/Desktop/GitHub/radiant_dev/dev/ ~/Desktop/GitHub/radiant_dev/inst/

rsync -av ~/Desktop/GitHub/radiant_dev/dev_branch/marketing/ ~/Desktop/GitHub/radiant_dev/inst/marketing/

# rsync -nav ~/Desktop/GitHub/radiant_dev/inst/  ~/Dropbox/radiant/inst/

# reverse it
# rsync -av ~/Dropbox/radiant/inst/ ~/Desktop/GitHub/radiant_dev/inst/

# rsync -nav ~/Desktop/GitHub/radiant_dev/inst/quant/  ~/Dropbox/radiant/inst/quant/
# rsync -nav ~/Dropbox/radiant/inst/quant/ ~/Desktop/GitHub/radiant_dev/inst/quant/

# rsync -av ~/Desktop/GitHub/radiant_dev/miniCRAN/*.R ~/Dropbox/radiant/miniCRAN/
# rsync -av ~/Desktop/GitHub/radiant_dev/launchers/ ~/Dropbox/radiant/launchers/
# rsync -av ~/Desktop/GitHub/radiant_dev/example_data/ ~/Dropbox/radiant/example_data/

# rsync -nauv ~/Desktop/GitHub/radiant_dev/miniCRAN/  ~/Dropbox/radiant/miniCRAN/

# rsync -av --delete ~/Desktop/GitHub/radiant_dev/launchers/ ~/Dropbox/radiant/launchers/
# diff -x ".DS_Store" -Naur ~/Desktop/GitHub/radiant_dev/inst/ ~/Desktop/GitHub/radiant_dev/dev/
