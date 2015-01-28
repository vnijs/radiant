cd ~/Desktop

sips -i radiant_green.png

# Extract the icon to its own resource file:
DeRez -only icns radiant_green.png > tmpicns.rsrc

# append this resource to the file you want to icon-ize.
Rez -append tmpicns.rsrc -o radiant_quant.command

# Use the resource to set the icon.
SetFile -a C radiant_quant.command

# clean up.
rm tmpicns.rsrc


sips -i radiant_red.png

# Extract the icon to its own resource file:
DeRez -only icns radiant_red.png > tmpicns.rsrc

# append this resource to the file you want to icon-ize.
Rez -append tmpicns.rsrc -o radiant_marketing.command

# Use the resource to set the icon.
SetFile -a C radiant_marketing.command

# clean up.
rm tmpicns.rsrc
