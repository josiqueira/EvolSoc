# https://stackoverflow.com/questions/17837289/break-exit-script
# Not pretty, but here is a way to implement an exit() command in R which works for me.
# answered May 10 '17 at 13:43
# by jochen

eiras.exit <- function() {
  .Internal(.invokeRestart(list(NULL, NULL), NULL))
}
