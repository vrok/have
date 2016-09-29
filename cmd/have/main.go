package main

import (
	"flag"
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"path"
	"strings"
	"syscall"

	"github.com/vrok/have/have"
)

// Implements PkgLocator
type FilesystemPkgLocator struct {
	gopath string
}

func NewFilesystemPkgLocator(gopath string) *FilesystemPkgLocator {
	return &FilesystemPkgLocator{gopath: gopath}
}

func (gpl *FilesystemPkgLocator) Locate(relativePath string) ([]*have.File, error) {
	var fullPkgPath = path.Join(gpl.gopath, relativePath)
	var flist, err = ioutil.ReadDir(fullPkgPath)
	if err != nil {
		return nil, err
	}

	var files []*have.File

	for _, f := range flist {
		if f.IsDir() {
			continue
		}

		if n := f.Name(); strings.HasSuffix(n, ".hav") {
			code, err := ioutil.ReadFile(path.Join(fullPkgPath, n))
			if err != nil {
				return nil, fmt.Errorf("Error reading %s: %s", n, err)
			}

			files = append(files, have.NewFile(path.Join(relativePath, f.Name()), string(code)))
		}
	}
	return files, nil
}

func paths() (gopath, srcpath string) {
	gopath = os.Getenv("GOPATH")
	if gopath == "" {
		fmt.Fprintf(os.Stderr, "GOPATH not set")
		os.Exit(1)
	}

	srcpath = os.Getenv("HAVESRCPATH")
	if srcpath == "" {
		srcpath = path.Join(gopath, "src")
	}

	return
}

func trans(args []string) {
	var pkgs, files []string
	for _, arg := range args {
		if strings.HasSuffix(arg, ".hav") {
			files = append(files, arg)
		} else {
			pkgs = append(pkgs, arg)
		}
	}

	var gopath, srcpath = paths()

	var locator = NewFilesystemPkgLocator(srcpath)

	manager := have.NewPkgManager(locator)

	for _, pkgName := range pkgs {
		pkg, errs := manager.Load(pkgName)

		for _, err := range errs {
			if compErr, ok := err.(*have.CompileError); ok {
				fmt.Fprintf(os.Stderr, "ERROR: %s\n", compErr.PrettyString(manager.Fset))
			} else {
				fmt.Fprintf(os.Stderr, "ERROR: %s\n", err)
			}
		}

		if len(errs) > 0 {
			os.Exit(1)
		}

		for _, f := range pkg.Files {
			if f.Name == have.BuiltinsFileName {
				continue
			}
			var output = f.GenerateCode()

			var fullFname = path.Join(gopath, "src", f.Name+".go")
			if strings.HasSuffix(f.Name, ".hav") {
				fullFname = path.Join(gopath, "src", f.Name[0:len(f.Name)-len("hav")]+"go")
			}

			os.MkdirAll(path.Dir(fullFname), 0744)

			var err = ioutil.WriteFile(fullFname, []byte(output), 0644)
			if err != nil {
				fmt.Fprintf(os.Stderr, "Error writing file %s: %s", fullFname, err)
				os.Exit(1)
			}
		}
	}
}

// Implements PkgLocator
type RunLocator struct {
	fpl          *FilesystemPkgLocator
	mainPkgFiles []*have.File
}

func NewRunLocator(fpl *FilesystemPkgLocator, mainFilenames []string) (*RunLocator, error) {
	var files []*have.File
	for _, f := range mainFilenames {
		code, err := ioutil.ReadFile(f)
		if err != nil {
			return nil, fmt.Errorf("Error reading %s: %s", f, err)
		}

		files = append(files, have.NewFile(f, string(code)))
	}
	return &RunLocator{
		fpl:          fpl,
		mainPkgFiles: files,
	}, nil
}

func (rl *RunLocator) Locate(relativePath string) ([]*have.File, error) {
	if relativePath == "main" {
		return rl.mainPkgFiles, nil
	}
	return rl.fpl.Locate(relativePath)
}

func run(args []string) {
	if len(args) == 0 {
		fmt.Fprintf(os.Stderr, "No source files specified\n")
		os.Exit(1)
	}

	for _, arg := range args {
		if !strings.HasSuffix(arg, ".hav") {
			fmt.Fprintf(os.Stderr, "Not a source file: %s", arg)
		}
	}

	var _, srcpath = paths()

	var locator, err = NewRunLocator(NewFilesystemPkgLocator(srcpath), args)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating temporary dir: %s", err)
		os.Exit(1)
	}

	manager := have.NewPkgManager(locator)

	pkg, errs := manager.Load("main")

	for _, err := range errs {
		if compErr, ok := err.(*have.CompileError); ok {
			fmt.Fprintf(os.Stderr, "ERROR: %s\n", compErr.PrettyString(manager.Fset))
		} else {
			fmt.Fprintf(os.Stderr, "ERROR: %s\n", err)
		}
	}

	if len(errs) > 0 {
		os.Exit(1)
	}

	tmpDir, err := ioutil.TempDir("", "hav")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating temporary dir: %s", err)
		os.Exit(1)
	}
	defer os.RemoveAll(tmpDir)

	var goFiles []string

	for _, f := range pkg.Files {
		if f.Name == have.BuiltinsFileName {
			continue
		}
		var output = f.GenerateCode()

		outputPath := path.Join(tmpDir, f.Name+".go")
		ioutil.WriteFile(outputPath, []byte(output), 0600)
		goFiles = append(goFiles, outputPath)
	}

	binary, err := exec.LookPath("go")
	if err != nil {
		fmt.Fprintf(os.Stderr, "Couldn't find go binary: %s", err)
		os.Exit(1)
	}

	env := os.Environ()

	err = syscall.Exec(binary, append([]string{"go", "run"}, goFiles...), env)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error execing go binary: %s", err)
		os.Exit(1)
	}
}

func main() {
	flag.CommandLine = flag.NewFlagSet(os.Args[0], flag.ContinueOnError)
	flag.Usage = func() {
		messages := map[string]string{
			"trans": "Translate .hav files to .go",
			"run":   "Run the translated versions of .hav files",
			"help":  "Print this help message",
		}
		fmt.Printf("Usage: have command [arguments]\n\n")
		fmt.Printf("The commands are: \n")
		for command, message := range messages {
			fmt.Printf("\t%s\t%s\n", command, message)
		}
	}
	flag.Parse()

	var args = flag.Args()

	if len(args) == 0 {
		fmt.Fprintf(os.Stderr, "Arguments missing\n")
		return
	}

	switch args[0] {
	case "trans":
		trans(args[1:])
	case "run":
		run(args[1:])
	case "help":
		flag.Usage()
	default:
		fmt.Fprintf(os.Stderr, "Unknown command: %s\n", args[0])
	}
}
