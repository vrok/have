package main

import (
	"flag"
	"fmt"
	"have/have"
	"io/ioutil"
	"os"
	"path"
	"strings"
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

			files = append(files, have.NewFile(f.Name(), string(code)))
		}
	}
	return files, nil
}

func trans(args []string) error {
	var pkgs, files []string
	for _, arg := range args {
		if strings.HasSuffix(arg, ".hav") {
			files = append(files, arg)
		} else {
			pkgs = append(pkgs, arg)
		}
	}

	var srcpath = os.Getenv("HAVESRCPATH")
	if srcpath == "" {
		var gopath = os.Getenv("GOPATH")
		if gopath == "" {
			return fmt.Errorf("Neither HAVESRCPATH nor GOPATH is set")
		}

		srcpath = path.Join(gopath, "src")
	}

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
			continue
		}

		for _, f := range pkg.Files {
			if f.Name == have.BuiltinsFileName {
				continue
			}
			output := f.GenerateCode()
			fmt.Printf("OUTPUT %s:\n%s\n", f.Name, output)
		}
	}

	return nil
}

func main() {
	flag.Parse()

	var args = flag.Args()

	if len(args) == 0 {
		fmt.Fprintf(os.Stderr, "Arguments missing\n")
		return
	}

	switch args[0] {
	case "trans":
		trans(args[1:])
	default:
		fmt.Fprintf(os.Stderr, "Unknown command: %s\n", args[0])
	}
}
