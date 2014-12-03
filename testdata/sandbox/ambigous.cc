# include <iostream>
# include <boost/program_options.hpp>

int main(int argc, char * argv[])
try {
    using namespace boost::program_options;

    options_description desc("Options");
    desc.add_options()
        ("foo", "foo")
        ("foobar", "foobar")
        ("foo-bar", "foo-bar")
        ("barbar", "barbar")
        ("bar-bar", "bar-bar")
        ("bar", "bar")
        ;
    parsed_options parsed = parse_command_line(argc, argv, desc);
    variables_map vm;
    store(parsed, vm);
    notify(vm);
    std::cerr << vm.count("foo") << std::endl;
    std::cerr << vm.count("foobar") << std::endl;
    std::cerr << vm.count("foo-bar") << std::endl;
    std::cerr << vm.count("bar") << std::endl;
    std::cerr << vm.count("barbar") << std::endl;
    std::cerr << vm.count("bar-bar") << std::endl;
} catch (std::exception & ex) {
    std::cerr << argv[0] << ": " << ex.what() << std::endl;
    return EXIT_FAILURE;
}
