bin = "elschemo"
names = %w[lisp]

task :build do
  sh "ghc --make -package parsec -fglasgow-exts -o #{bin} #{extensionize 'hs', names}"
end

task :clean do
  sh "rm -f #{bin} #{obj_files(names)}"
end

def obj_files names
  "#{extensionize 'hi', names} #{extensionize 'o', names}"
end

def extensionize ext, names
  names.join(".#{ext} ") + ".#{ext}"
end



# bin = "elschemo"
# names = %w[main elschemo parser eval numeric primitives io]
# 
# task :build do
#   sh "ghc --make -package parsec -fglasgow-exts -o #{bin} #{extensionize 'hs', names}"
# end
# 
# task :clean do
#   sh "rm -f #{bin} #{obj_files(names)}"
# end
# 
# def obj_files names
#   "#{extensionize 'hi', names} #{extensionize 'o', names}"
# end
# 
# def extensionize ext, names
#   names.join(".#{ext} ") + ".#{ext}"
# end
# 
