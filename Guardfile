guard :shell do
  watch(/(.*).erl/) do |m|
    compile(m[0]) and run_tests(m[1])
    puts
  end
end

def compile(path)
  puts "compile #{path}"
  puts `erlc #{path}`
  $? == 0
end

def run_tests(name)
  name = name.sub('_test', '')

  if (File.exists?("#{name}_test.erl"))
    puts "test #{name}"
    puts `erl -noshell -eval "eunit:test(#{name}_test, [verbose])" -s init stop`
  end
end