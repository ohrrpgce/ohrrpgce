// This is a quick hack. The proper fix is to merge in the -entry argument to fbc
// to specify the name of the entry function from the Mac port fork

int main(int argc, char **argv);

int SDL_main(int argc, char **argv) {
    return main(argc, argv);
}
