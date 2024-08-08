//! Cmdline
//!
//! Simple command line parser.
//!
const std = @import("std");

pub const Options = struct {
    alloc: std.mem.Allocator,

    /// positional arguments, called 'words'
    _words: std.ArrayList([]const u8),

    /// all options keyed by their full names (e.g. --verbose)
    _items: std.StringHashMap(*Option),

    /// subset of options which also have a short name (e.g. -v)
    _shorts: std.AutoHashMap(u8, *Option),

    /// process arguments to deallocate in deinit
    _args: ?std.process.ArgIterator = null,

    /// Initialize Options struct. Caller must call deinit to release
    /// internal buffers when finished. OPTIONS is a tuple of tuples.
    pub fn init(alloc: std.mem.Allocator, comptime options: anytype) error{ BadArgument, OutOfMemory }!Options {
        const ti = @typeInfo(@TypeOf(options));
        if (ti != .Struct or !ti.Struct.is_tuple) return error.BadArgument;

        const OptionCreateOpts = struct {
            /// The name, used as the long option, e.g. --name
            name: ?[]const u8 = null,

            /// By default, will use first character of NAME. Supply NULL to specify no short option.
            short: ?u8 = ' ',

            /// Set to true if this is a boolean option with no value captured.
            boolean: bool = false,
        };

        var items = std.StringHashMap(*Option).init(alloc);
        var shorts = std.AutoHashMap(u8, *Option).init(alloc);
        const words = std.ArrayList([]const u8).init(alloc);

        inline for (options) |one| {
            const one_ti = @typeInfo(@TypeOf(one));
            if (one_ti != .Struct or !one_ti.Struct.is_tuple) return error.BadArgument;

            var create_opts = OptionCreateOpts{};

            inline for (one) |x| {
                const x_ty = @TypeOf(x);
                const x_ti = @typeInfo(x_ty);

                if (x_ty == comptime_int) {
                    const c: u8 = x;
                    if (c == 0) {
                        create_opts.short = null;
                    } else {
                        create_opts.short = c;
                    }
                    continue;
                }
                if (x_ty == bool) {
                    create_opts.boolean = true;
                    continue;
                }
                if (x_ty == []const u8) {
                    create_opts.name = x;
                    continue;
                }
                if (x_ti == .Pointer) {
                    if (@typeInfo(x_ti.Pointer.child) == .Array) {
                        create_opts.name = x;
                        continue;
                    }
                }

                // std.debug.print("Expected a slice, bool or comptime_int, got: {}\n", .{@TypeOf(x)});
                return error.BadArgument;
            }

            // name must exist and be more than one character
            if (create_opts.name == null or create_opts.name.?.len < 2)
                return error.BadArgument;

            // default short option is first character of name.
            if (create_opts.short == ' ')
                create_opts.short = create_opts.name.?[0];

            const option = try alloc.create(Option);

            if (create_opts.name) |name| {
                option.init(name, create_opts.boolean);
                try items.put(name, option);
            }

            if (create_opts.short) |s| {
                try shorts.put(s, option);
            }

            // set default boolean value - an odd use case but allows
            // init to accept 'true' for options which should always
            // be 'present'
            option.present = create_opts.boolean;
        }

        return .{
            .alloc = alloc,
            ._items = items,
            ._shorts = shorts,
            ._words = words,
        };
    }

    pub fn deinit(self: *Options) void {

        // destroy options
        var it = self._items.iterator();
        while (it.next()) |e| {
            self.alloc.destroy(e.value_ptr.*);
        }

        // words are slices into the process argument memory, with
        // lifetime equal to the lifetime of the
        // self.process.ArgIterator that is allocated in parse()
        self._words.deinit();

        self._shorts.deinit();
        self._items.deinit();

        if (self._args) |*args|
            args.deinit();
    }

    /// Parse command line arguments. Allocates for process args using
    /// its own allocator (provided in init).
    pub fn parse(self: *Options) !ParseResult {
        self._args = try std.process.argsWithAllocator(self.alloc);
        return _parse(self, &self._args.?);
    }

    /// Get a previously created option by its name.
    pub fn getOption(self: *const Options, name: []const u8) ?*const Option {
        if (self._items.get(name)) |o| return o;
        if (name.len == 1) return self._shorts.get(name[0]);
        return null;
    }

    /// Get an option's value if it is present. For boolean options
    /// that are present, this will be an empty slice. If the option
    /// is not present, null is returned.
    fn get(self: *const Options, name: []const u8) ?[]const u8 {
        if (self.getOption(name)) |o|
            // if option is present, return its value (a string), or
            // if its value is null, return an empty slice.
            if (o.present) if (o.value) |x| return x else return "";
        return null;
    }

    /// Return true if option is present.
    pub fn present(self: *const Options, name: []const u8) bool {
        if (self.getOption(name)) |o| return o.present;
        return false;
    }

    /// Get positional arguments as a const ArrayList of slices.
    pub fn positional(self: *const Options) *const std.ArrayList([]const u8) {
        return &self._words;
    }

    pub fn short(self: *Options, short_name: u8, option: *Option) !void {
        var found: ?*Option = undefined;
        var it = self._items.iterator();
        while (it.next()) |e| {
            if (e.value_ptr.* == option) {
                found = option;
                break;
            }
        }

        if (found) |opt| {
            try self._shorts.put(short_name, opt);
        } else return error.NotFound;
    }

    /// Reset all options to present = false for testing purposes, and clear words list
    pub fn reset(self: *Options) void {
        var it = self._items.iterator();
        while (it.next()) |e| {
            e.value_ptr.*.reset();
        }
        self._words.clearAndFree();
    }
};

pub const Option = struct {
    // public fields
    name: []const u8,

    /// union with the argument value. It is initialised to an empty
    /// slice. For boolean options, this is NULL, and the PRESENT
    /// field is used instead.
    value: ?[]const u8 = "",

    /// True if the option was seen on the command line and its
    /// argument (if any) was valid.
    present: bool = false,

    // private fields

    /// Initialise or reset an existing Option tagged TAG.
    pub fn init(self: *Option, name: []const u8, boolean: bool) void {
        if (boolean == true) {
            self.* = Option{ .name = name, .value = null };
        } else {
            self.* = Option{ .name = name, .value = "" };
        }

        self.reset();
    }

    /// Return true if this is a boolean option
    pub fn isBoolean(self: Option) bool {
        return self.value == null;
    }

    /// Reset option to not being present, for testing purposes.
    pub fn reset(self: *Option) void {
        self.present = false;
    }

    /// load a value into option from a string.
    pub fn loadFromString(self: *Option, value: []const u8) void {
        // assume success
        self.present = true;
        self.value = value;
    }
};

/// Tagged union result of a parse operation (check .ok or .err)
const ParseResult = union(enum) {
    ok: struct {},
    err: union(enum) {
        /// a non-boolean flag was observed in a packed flag word (e.g. -itf)
        packedNonBooleanFlag: u8,

        /// attempt to assign a value to a boolean, e.g. --verbose=123
        booleanWithValue: []const u8,

        /// an unknown (single-letter) flag was observed
        unknownFlag: u8,

        /// an unknown (multi-character) option was observed
        unknownOption: []const u8,

        /// option is missing its argument
        missingArgument: []const u8,
    },
};

/// args must be an iterator with a next() function
fn _parse(options: *Options, args: anytype) !ParseResult {
    var end_of_options: bool = false; // seen '--' ?

    while (args.next()) |v| {
        // form: positional argument
        if (end_of_options or !std.mem.startsWith(u8, v, "-")) {
            // positional argument
            try options._words.append(v);
            continue;
        }

        if (std.mem.startsWith(u8, v, "--")) {
            // form: -- (we are done parsing options)
            if (v.len == 2) {
                end_of_options = true;
                continue;
            }

            const key = v[2..]; // chars after '--'

            // form: --key=value
            if (std.mem.indexOfScalar(u8, key, '=')) |i| {
                const realised_key = key[0..i];
                const val = key[i + 1 ..]; // skip '='

                const res = handleKeyEqualsValue(options, realised_key, val);
                if (res == .ok) continue else return res;
            }

            // form: --key value
            const res = handleKeyConsumesNext(options, key, args);
            if (res == .ok) continue else return res;
        }

        // form: -x
        const key = v[1]; // char after '-'

        if (options._shorts.get(key)) |opt| {
            // if opt is a boolean flag check for packed boolean flags
            if (opt.isBoolean()) {
                // v[1..] captures all the chars after '-'
                const res = handlePackedBoolean(options, v[1..], args);
                if (res == .ok) continue else return res;
            }

            const res = handleSingleFlag(opt, v[1..], args);
            if (res == .ok) continue else return res;
        }
        return .{ .err = .{ .unknownFlag = key } };
    }
    return .{ .ok = .{} };
}

fn handleKeyEqualsValue(options: *Options, key: []const u8, val: []const u8) ParseResult {
    if (options._items.get(key)) |opt| {
        if (!opt.isBoolean()) {
            opt.loadFromString(val);
            return .{ .ok = .{} };
        }
        return .{ .err = .{ .booleanWithValue = key } };
    }
    return .{ .err = .{ .unknownOption = key } };
}

fn handleKeyConsumesNext(options: *Options, key: []const u8, args: anytype) ParseResult {
    if (options._items.get(key)) |opt| {
        if (opt.isBoolean()) {
            opt.present = true;
            return .{ .ok = .{} };
        }

        if (args.next()) |next| {
            opt.loadFromString(next);
            return .{ .ok = .{} };
        }

        return .{ .err = .{ .missingArgument = opt.name } };
    }
    return .{ .err = .{ .unknownOption = key } };
}

fn handlePackedBoolean(options: *Options, flags: []const u8, args: anytype) ParseResult {
    for (flags, 1..) |x, pos| {
        if (options._shorts.get(x)) |xopt| {
            // iterate one character at a time

            if (xopt.isBoolean()) {
                xopt.present = true;
                continue;
            }

            // a non-boolean flag is only allowed if it's the last one
            if (pos != flags.len)
                return .{ .err = .{ .packedNonBooleanFlag = x } };

            // consume next argument.
            // intentially do not support = for the last flag in a packed set
            if (args.next()) |next| {
                xopt.loadFromString(next);
                return .{ .ok = .{} };
            }
            return .{ .err = .{ .missingArgument = xopt.name } };
        }
        return .{ .err = .{ .unknownFlag = x } };
    }

    // all flags were boolean and processed
    return .{ .ok = .{} };
}

fn handleSingleFlag(
    opt: *Option,
    key: []const u8,
    args: anytype,
) ParseResult {
    // form: -o=filename.txt
    if (std.mem.indexOfScalar(u8, key, '=')) |i| {
        const val = key[i + 1 ..];
        if (val.len == 0)
            return .{ .err = .{ .missingArgument = opt.name } };
        opt.loadFromString(val);
        return .{ .ok = .{} };
    }

    // form: -ofilename.txt
    if (key.len > 1) {
        opt.loadFromString(key[1..]);
        return .{ .ok = .{} };
    }

    // form: -o filename.txt
    if (args.next()) |next| {
        opt.loadFromString(next);
        return .{ .ok = .{} };
    }

    // ran out of arguments
    return .{ .err = .{ .missingArgument = opt.name } };
}

test "typical command" {
    const expect = std.testing.expect;
    const alloc = std.testing.allocator;

    var options = try Options.init(alloc, .{
        .{"file"}, .{ "verbose", false }, .{"db"},
    });
    defer options.deinit();

    const t1 = "create --file foo.txt --verbose --db ./db";
    {
        var res = try testCmdline(alloc, t1, &options);
        defer res.iterator.deinit();
        try expect(std.mem.eql(u8, options.get("file").?, "foo.txt"));
        try expect(std.mem.eql(u8, options.get("db").?, "./db"));
        try expect(options.present("verbose") == true);
        try expect(options.present("v") == true);

        try expect(options.positional().items.len == 1);
        try expect(std.mem.eql(u8, options.positional().items[0], "create"));
    }

    // -f argument is packed (-fhello)
    {
        options.reset();
        var res = try testCmdline(alloc, "create -fhello.txt", &options);
        defer res.deinit();
        try expect(res.parseResult == .ok);
        try expect(options.present("file") == true);
        try expect(std.mem.eql(u8, options.get("file").?, "hello.txt"));
    }

    // -f argument is packed and quoted (-f"hello world")
    {
        options.reset();
        var res = try testCmdline(alloc, "create -f\"hello world\"", &options);
        defer res.deinit();
        try expect(res.parseResult == .ok);
        try expect(options.present("file") == true);
        try expect(std.mem.eql(u8, options.get("file").?, "hello world"));
    }

    // --file argument is missing
    {
        options.reset();
        const t2 = "create foo.txt -v";
        var res = try testCmdline(alloc, t2, &options);
        defer res.deinit();
        try expect(options.get("file") == null);

        try expect(options.positional().items.len == 2);
        try expect(std.mem.eql(u8, options.positional().items[0], "create"));
        try expect(std.mem.eql(u8, options.positional().items[1], "foo.txt"));
    }

    // --file is present but has no value
    {
        options.reset();
        const t3 = "create --file   ";
        var res = try testCmdline(alloc, t3, &options);
        defer res.deinit();

        try expect(res.parseResult == .err);
        try expect(res.parseResult.err == .missingArgument);
        try expect(std.mem.eql(u8, res.parseResult.err.missingArgument, "file"));

        try expect(options.get("file") == null);
        try expect(options.get("f") == null);
    }

    // --file argument uses --file=value form
    {
        options.reset();
        const t = "create --file=foo.txt";
        var res = try testCmdline(alloc, t, &options);
        defer res.deinit();
        try expect(res.parseResult == .ok);
        try expect(std.mem.eql(u8, options.get("file").?, "foo.txt"));
    }

    // -f argument uses -f=value form
    {
        options.reset();
        const t = "create -f=foo.txt";
        var res = try testCmdline(alloc, t, &options);
        defer res.deinit();
        try expect(res.parseResult == .ok);
        try expect(std.mem.eql(u8, options.get("file").?, "foo.txt"));
    }
}

test "use slice instead of static string" {
    const expect = std.testing.expect;
    const alloc = std.testing.allocator;

    const file_static = "file";
    const file_slice: []const u8 = file_static[0..file_static.len];

    var options = try Options.init(alloc, .{
        .{file_slice},
    });
    defer options.deinit();

    var res = try testCmdline(alloc, "create --file foo.txt", &options);
    defer res.iterator.deinit();
    try expect(std.mem.eql(u8, options.get("file").?, "foo.txt"));
}

test "flag packing" {
    const expect = std.testing.expect;
    const alloc = std.testing.allocator;
    var options = try Options.init(alloc, .{
        .{ "i-flag", false },
        .{ "t-flag", false },
        .{ "f-flag", false },
    });
    defer options.deinit();

    {
        var res = try testCmdline(alloc, "-itf", &options);
        defer res.deinit();
        try expect(options.present("i-flag") == true);
        try expect(options.present("t-flag") == true);
        try expect(options.present("f-flag") == true);
    }
    {
        options.reset();
        var res = try testCmdline(alloc, "-i", &options);
        defer res.deinit();
        try expect(options.present("i-flag") == true);
    }
}

test "mixed flag packing" {
    const expect = std.testing.expect;
    const eql = std.mem.eql;
    const alloc = std.testing.allocator;
    var options = try Options.init(alloc, .{
        .{ "extract", 'x', false },
        .{ "verify", false },
        .{"file"},
    });
    defer options.deinit();

    {
        var res = try testCmdline(alloc, "-xvf foo.txt", &options);
        defer res.deinit();
        try expect(res.parseResult == .ok);
        try expect(options.present("extract") == true);
        try expect(options.present("verify") == true);
        try expect(eql(u8, options.get("file").?, "foo.txt"));
    }
}

test "parse errors" {
    const expect = std.testing.expect;
    const alloc = std.testing.allocator;
    const eql = std.mem.eql;
    var options = try Options.init(alloc, .{
        .{ "b-flag", false },
        .{"i-flag"},
    });
    defer options.deinit();

    {
        options.reset();
        var res = try testCmdline(alloc, "-x", &options);
        defer res.deinit();

        try expect(res.parseResult == .err);
        try expect(res.parseResult.err == .unknownFlag);
        try expect(res.parseResult.err.unknownFlag == 'x');
    }
    {
        options.reset();
        var res = try testCmdline(alloc, "-bx", &options);
        defer res.deinit();

        try expect(res.parseResult == .err);
        try expect(res.parseResult.err == .unknownFlag);
        try expect(res.parseResult.err.unknownFlag == 'x');
    }
    {
        var res = try testCmdline(alloc, "-bi", &options);
        defer res.deinit();

        try expect(res.parseResult == .err);
        try expect(res.parseResult.err == .missingArgument);
        try expect(eql(u8, res.parseResult.err.missingArgument, "i-flag"));
    }
}

test "args after --" {
    const expect = std.testing.expect;
    const alloc = std.testing.allocator;
    var options = try Options.init(alloc, .{});
    defer options.deinit();

    var res = try testCmdline(alloc, "-- a b c --long", &options);
    defer res.deinit();
    try expect(res.parseResult == .ok);
    try expect(options.positional().items.len == 4);
    try expect(std.mem.eql(u8, options.positional().items[0], "a"));
    try expect(std.mem.eql(u8, options.positional().items[1], "b"));
    try expect(std.mem.eql(u8, options.positional().items[2], "c"));
    try expect(std.mem.eql(u8, options.positional().items[3], "--long"));
}

test "suppress short option" {
    const expect = std.testing.expect;
    const alloc = std.testing.allocator;
    var options = try Options.init(alloc, .{
        .{ "special", 0 },
    });
    defer options.deinit();

    var res = try testCmdline(alloc, "-s hello", &options);
    defer res.deinit();
    try expect(!options.present("special"));
}

test "error on single-character string" {
    // should use a u8 instead
    const alloc = std.testing.allocator;
    try std.testing.expectError(error.BadArgument, Options.init(alloc, .{
        .{ "extract", "x" },
    }));
}

const TestCmdlineResult = struct {
    iterator: std.process.ArgIteratorGeneral(.{}),
    parseResult: ParseResult,

    pub fn deinit(self: *TestCmdlineResult) void {
        self.iterator.deinit();
    }
};

/// caller must `deinit` the return value
fn testCmdline(
    alloc: std.mem.Allocator,
    s: []const u8,
    options: *Options,
) !TestCmdlineResult {
    var it = try std.process.ArgIteratorGeneral(.{}).init(alloc, s);
    const res = try _parse(options, &it);
    return .{ .iterator = it, .parseResult = res };
}
