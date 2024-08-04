# Cmdline

Simple command line parser.

# Installation
Add to your `build.zig.zon`:
```sh
zig fetch --save https://github.com/anticrisis/zig-cmdline/archive/refs/tags/v0.1.0.tar.gz
```

Then include in your `build.zig`:

```zig
    const cmdline = b.dependency("cmdline", .{
        .target = target,
        .optimize = dep_optimize,
    }).module("cmdline");

    exe.root_module.addImport("cmdline", cmdline);
```

# Features

- Valid option styles
  - `--long argument`
  - `-l argument`
  - `--long=argument`
  - `-l=argument`
  - `--boolean` (no argument)
  - `-b`
  - `-abc` (packed boolean flags, equiv. to `-a -b -c`)
  - `-xvf file.txt` (packed bools followed by string option, equiv. to
    `-x -v -f=file.txt`)
  - `--` stops all option parsing, any remaining arguments are
    considered positional

- Positional arguments
  - everything without a dash is positional, can be interleaved with
    options

- Short option character can be different from first letter of long
  option, e.g. (`--extract` and `-x` can be the same option).

- Typed values `i64` or `f64`

- Panics on out of memory errors during configuration, to make API a
  bit easier to use.

- Does not check for required arguments or print usage or errors.


# Usage
```zig
// provide an allocator
var options = try options.init(alloc);
defer options.deinit();

// creates --extract and -x as the same option, which has no value
_ = options.create("extract", .boolean).short('x');

// creates --file and -f in one call, which has a string value
_ = options.createShort("file", .string);

// could retain the return values above. They are *Option, which will
// be filled during parse. But possibly easier to use get("name") API as
// shown below.

// parse the command line arguments from std.os.process
const result = options.parse();

if (result == .err) {
    // switch on .err for detailed error information, and/or
    // print usage and exit
}

if (options.getString("file")) |filename| { ... }
if (options.present("extract")) { ... }
for (options.positional().items) |pos| { ... }
```

# Alternatives

Several libraries already exist and provide more or different
features. A selection is below, along with notable differences to this
library. See [Awesome Zig](https://github.com/zigcc/awesome-zig) for more.

## zig-clap
https://github.com/Hejsil/zig-clap

- Supports options that can be specified multiple times
- Prints help message
- Parses help message to specify command line options
- Provides autodoc

## Zig Argument Parser
https://github.com/ikskuh/zig-args

- Config via a struct
- Supports booleans with values (yes/no, etc)
- Supports enumerations

## Zigcli
https://github.com/jiacai2050/zigcli

- Provides several example executables

## Yazap
https://github.com/PrajwalCH/yazap

- Supports options that can be specified multiple times
- Provides structured support for nested subcommands with positional
  arguments
