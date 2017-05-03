## CSS configuration / creation
# apt install ruby
# gem install sass

export RESOURCES="../resources/html/"

cd $RESOURCES

sass-convert soundboard.sass soundboard.scss
sass soundboard.scss soundboard.css
