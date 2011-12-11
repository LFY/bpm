BEGIN { print "(define grammar-sequence '(" }
(!($1 == "#") && !($1 == "depth:") && !($1 == "posterior:")) { print $0 }
END { print "))" }
