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
    /// internal buffers when finished.
    pub fn init(alloc: std.mem.Allocator) !Options {
        return .{
            .alloc = alloc,
            ._items = std.StringHashMap(*Option).init(alloc),
            ._shorts = std.AutoHashMap(u8, *Option).init(alloc),
            ._words = std.ArrayList([]const u8).init(alloc),
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
    pub fn parse(self: *Options) ParseResult {
        self._args = std.process.argsWithAllocator(self.alloc) catch @panic("OOM");
        return _parse(self, &self._args.?);
    }

    /// Create an option of the given type tag.
    pub fn create(self: *Options, name: []const u8, tag: Option.Tag) *Option {
        const alloc = self.alloc;
        const option = alloc.create(Option) catch @panic("OOM");
        option.init(self, name, tag);
        self._items.put(name, option) catch @panic("OOM");
        return option;
    }

    /// create a short-named option using the first u8 of NAME.
    pub fn createShort(self: *Options, name: []const u8, tag: Option.Tag) *Option {
        if (name.len < 1) @panic("name too short");
        const option = self.create(name, tag);
        return option.short(name[0]);
    }

    /// Get a previously created option by its name.
    pub fn get(self: *const Options, name: []const u8) ?*const Option {
        if (self._items.get(name)) |o| return o;
        if (name.len == 1) return self._shorts.get(name[0]);
        return null;
    }

    /// Get an option's value if it is present.
    fn getValue(self: *const Options, name: []const u8) ?Option.T {
        if (self.get(name)) |o| if (o.present) return o.value;
        return null;
    }

    /// Get an option's string value if it is present.
    pub fn getString(self: *const Options, name: []const u8) ?[]const u8 {
        if (self.getValue(name)) |v| if (v == .string) return v.string;
        return null;
    }

    /// Get an option's i64 value if it is present.
    pub fn getInt(self: *const Options, name: []const u8) ?i64 {
        if (self.getValue(name)) |v| if (v == .int) return v.int;
        return null;
    }

    /// Get an option's f64 value if it is present.
    pub fn getFloat(self: *const Options, name: []const u8) ?f64 {
        if (self.getValue(name)) |v| if (v == .float) return v.float;
        return null;
    }

    /// Return true if option is present.
    pub fn present(self: *const Options, name: []const u8) bool {
        if (self.get(name)) |o| return o.present;
        return false;
    }

    /// Get positional arguments as a const ArrayList of slices.
    pub fn positional(self: *const Options) *const std.ArrayList([]const u8) {
        return &self._words;
    }

    pub fn short(self: *Options, short_name: u8, option: *Option) void {
        var found: ?*Option = undefined;
        var it = self._items.iterator();
        while (it.next()) |e| {
            if (e.value_ptr.* == option) {
                found = option;
                break;
            }
        }
        if (found) |opt| {
            self._shorts.put(short_name, opt) catch @panic("OOM");
        } else @panic("option not found");
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

    /// union with the argument value
    value: T,

    /// True if the option was seen on the command line and its
    /// argument (if any) was valid.
    present: bool = false,

    // private fields
    _parent: *Options,

    /// Specifies the type of the option
    const Tag = enum { int, float, string, boolean };

    /// Contains the argument value
    const T = union(Tag) {
        int: i64,
        float: f64,
        string: []const u8,

        /// For a boolean, the option's `present` field is determinative.
        boolean: struct {},
    };

    /// Initialise or reset an existing Option tagged TAG.
    pub fn init(self: *Option, parent: *Options, name: []const u8, tag: Tag) void {
        self.* = switch (tag) {
            .int => Option{
                .name = name,
                ._parent = parent,
                .value = .{ .int = 0 },
            },
            .float => Option{
                .name = name,
                ._parent = parent,
                .value = .{ .float = 0.0 },
            },
            .string => Option{
                .name = name,
                ._parent = parent,
                .value = .{ .string = "" },
            },
            .boolean => Option{
                .name = name,
                ._parent = parent,
                .value = .{ .boolean = .{} },
            },
        };
        self.reset();
    }

    /// Reset option to not being present, for testing purposes.
    pub fn reset(self: *Option) void {
        self.present = false;
    }

    /// Add a short name to this option.
    pub fn short(self: *Option, short_name: u8) *Option {
        self._parent.short(short_name, self);
        return self;
    }

    /// Load a type-checked value into option from a string, if
    /// possible.
    pub fn loadFromString(self: *Option, value: []const u8) void {
        // assume success
        self.present = true;

        switch (self.value) {
            .int => if (std.fmt.parseInt(i64, value, 0)) |n| {
                self.value.int = n;
            } else |_| {
                self.present = false;
            },
            .float => if (std.fmt.parseFloat(f64, value)) |n| {
                self.value.float = n;
            } else |_| {
                self.present = false;
            },
            .string => {
                self.value.string = value;
            },
            .boolean => {
                self.value.boolean = .{};
            },
        }
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
fn _parse(options: *Options, args: anytype) ParseResult {
    var end_of_options: bool = false; // seen '--' ?

    while (args.next()) |v| {
        // form: positional argument
        if (end_of_options or !std.mem.startsWith(u8, v, "-")) {
            // positional argument
            options._words.append(v) catch @panic("OOM");
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
            if (opt.value == .boolean) {
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
        if (opt.value != .boolean) {
            opt.loadFromString(val);
            return .{ .ok = .{} };
        }
        return .{ .err = .{ .booleanWithValue = key } };
    }
    return .{ .err = .{ .unknownOption = key } };
}

fn handleKeyConsumesNext(options: *Options, key: []const u8, args: anytype) ParseResult {
    if (options._items.get(key)) |opt| {
        if (opt.value == .boolean) {
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

            if (xopt.value == .boolean) {
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

    var options = try Options.init(alloc);
    defer options.deinit();

    const t1 = "create --file foo.txt --verbose --db ./db";
    _ = options.create("file", .string).short('f');
    _ = options.create("verbose", .boolean).short('v');
    _ = options.create("db", .string);
    {
        var res = testCmdline(alloc, t1, &options);
        defer res.iterator.deinit();
        try expect(std.mem.eql(u8, options.getString("file").?, "foo.txt"));
        try expect(std.mem.eql(u8, options.getString("db").?, "./db"));
        try expect(options.present("verbose") == true);
        try expect(options.present("v") == true);

        try expect(options.positional().items.len == 1);
        try expect(std.mem.eql(u8, options.positional().items[0], "create"));

        // retrieve with wrong type returns null
        try (expect(options.getInt("file") == null));
        try (expect(options.getFloat("f") == null));
    }

    // -f argument is packed (-fhello)
    {
        options.reset();
        var res = testCmdline(alloc, "create -fhello.txt", &options);
        defer res.deinit();
        try expect(res.parseResult == .ok);
        try expect(options.present("file") == true);
        try expect(std.mem.eql(u8, options.getString("file").?, "hello.txt"));
    }

    // -f argument is packed and quoted (-f"hello world")
    {
        options.reset();
        var res = testCmdline(alloc, "create -f\"hello world\"", &options);
        defer res.deinit();
        try expect(res.parseResult == .ok);
        try expect(options.present("file") == true);
        try expect(std.mem.eql(u8, options.getString("file").?, "hello world"));
    }

    // --file argument is missing
    {
        options.reset();
        const t2 = "create foo.txt -v";
        var res = testCmdline(alloc, t2, &options);
        defer res.deinit();
        try expect(options.getString("file") == null);

        try expect(options.positional().items.len == 2);
        try expect(std.mem.eql(u8, options.positional().items[0], "create"));
        try expect(std.mem.eql(u8, options.positional().items[1], "foo.txt"));
    }

    // --file is present but has no value
    {
        options.reset();
        const t3 = "create --file   ";
        var res = testCmdline(alloc, t3, &options);
        defer res.deinit();

        try expect(res.parseResult == .err);
        try expect(res.parseResult.err == .missingArgument);
        try expect(std.mem.eql(u8, res.parseResult.err.missingArgument, "file"));

        try expect(options.getString("file") == null);
        try expect(options.getString("f") == null);
    }

    // --file argument uses --file=value form
    {
        options.reset();
        const t = "create --file=foo.txt";
        var res = testCmdline(alloc, t, &options);
        defer res.deinit();
        try expect(res.parseResult == .ok);
        try expect(std.mem.eql(u8, options.getString("file").?, "foo.txt"));
    }
    // -f argument uses -f=value form
    {
        options.reset();
        const t = "create -f=foo.txt";
        var res = testCmdline(alloc, t, &options);
        defer res.deinit();
        try expect(res.parseResult == .ok);
        try expect(std.mem.eql(u8, options.getString("file").?, "foo.txt"));
    }
}

test "short with argument" {
    const expect = std.testing.expect;
    const alloc = std.testing.allocator;
    var options = try Options.init(alloc);
    defer options.deinit();

    _ = options.createShort("intval", .int);
    {
        options.reset();
        var res = testCmdline(alloc, "-i123", &options);
        defer res.deinit();
        try expect(options.getInt("intval") == 123);
        try expect(options.getInt("intval").? == 123);
    }
    {
        options.reset();
        var res = testCmdline(alloc, "-i 123", &options);
        defer res.deinit();
        try expect(options.getInt("intval") == 123);
        try expect(options.getInt("intval").? == 123);
    }
    _ = options.createShort("floatval", .float);
    {
        options.reset();
        var res = testCmdline(alloc, "-f123.456", &options);
        defer res.deinit();
        try expect(options.getFloat("floatval") == 123.456);
    }
    _ = options.createShort("strval", .string);
    {
        options.reset();
        var res = testCmdline(alloc, "-s\"quoted string\"", &options);
        defer res.deinit();
        try expect(std.mem.eql(u8, options.getString("strval").?, "quoted string"));
    }
    {
        options.reset();
        var res = testCmdline(alloc, "-s \"hello string\"", &options);
        defer res.deinit();
        try expect(std.mem.eql(u8, options.getString("strval").?, "hello string"));
    }
}

test "flag packing" {
    const expect = std.testing.expect;
    const alloc = std.testing.allocator;
    var options = try Options.init(alloc);
    defer options.deinit();

    _ = options.createShort("i-flag", .boolean);
    _ = options.createShort("t-flag", .boolean);
    _ = options.createShort("f-flag", .boolean);
    {
        var res = testCmdline(alloc, "-itf", &options);
        defer res.deinit();
        try expect(options.present("i-flag") == true);
        try expect(options.present("t-flag") == true);
        try expect(options.present("f-flag") == true);
    }
    {
        options.reset();
        var res = testCmdline(alloc, "-i", &options);
        defer res.deinit();
        try expect(options.present("i-flag") == true);
    }
}

test "mixed flag packing" {
    const expect = std.testing.expect;
    const eql = std.mem.eql;
    const alloc = std.testing.allocator;
    var options = try Options.init(alloc);
    defer options.deinit();

    _ = options.create("extract", .boolean).short('x');
    _ = options.create("verify", .boolean).short('v');
    _ = options.create("file", .string).short('f');

    {
        var res = testCmdline(alloc, "-xvf foo.txt", &options);
        defer res.deinit();
        try expect(res.parseResult == .ok);
        try expect(options.present("extract") == true);
        try expect(options.present("verify") == true);
        try expect(eql(u8, options.getString("file").?, "foo.txt"));
    }
}

test "parse errors" {
    const expect = std.testing.expect;
    const alloc = std.testing.allocator;
    const eql = std.mem.eql;
    var options = try Options.init(alloc);
    defer options.deinit();

    _ = options.createShort("b-flag", .boolean);
    _ = options.createShort("i-flag", .int);
    {
        options.reset();
        var res = testCmdline(alloc, "-x", &options);
        defer res.deinit();

        try expect(res.parseResult == .err);
        try expect(res.parseResult.err == .unknownFlag);
        try expect(res.parseResult.err.unknownFlag == 'x');
    }
    {
        options.reset();
        var res = testCmdline(alloc, "-bx", &options);
        defer res.deinit();

        try expect(res.parseResult == .err);
        try expect(res.parseResult.err == .unknownFlag);
        try expect(res.parseResult.err.unknownFlag == 'x');
    }
    {
        var res = testCmdline(alloc, "-bi", &options);
        defer res.deinit();

        try expect(res.parseResult == .err);
        try expect(res.parseResult.err == .missingArgument);
        try expect(eql(u8, res.parseResult.err.missingArgument, "i-flag"));
    }
}

test "wrong types" {
    const expect = std.testing.expect;
    const alloc = std.testing.allocator;
    var options = try Options.init(alloc);
    defer options.deinit();
    _ = options.create("int", .int);
    {
        var res = testCmdline(alloc, "--int 123.456", &options);
        defer res.deinit();
        try expect(options.getValue("int") == null);
    }
    _ = options.create("float", .float);
    {
        var res = testCmdline(alloc, "--float hello", &options);
        defer res.deinit();
        try expect(options.getValue("float") == null);
    }
    _ = options.create("str", .string);
    {
        var res = testCmdline(alloc, "--float 123", &options);
        defer res.deinit();
        try expect(options.getValue("str") == null);
    }
}

test "args after --" {
    const expect = std.testing.expect;
    const alloc = std.testing.allocator;
    var options = try Options.init(alloc);
    defer options.deinit();

    var res = testCmdline(alloc, "-- a b c --long", &options);
    defer res.deinit();
    try expect(res.parseResult == .ok);
    try expect(options.positional().items.len == 4);
    try expect(std.mem.eql(u8, options.positional().items[0], "a"));
    try expect(std.mem.eql(u8, options.positional().items[1], "b"));
    try expect(std.mem.eql(u8, options.positional().items[2], "c"));
    try expect(std.mem.eql(u8, options.positional().items[3], "--long"));
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
) TestCmdlineResult {
    var it = std.process.ArgIteratorGeneral(.{}).init(alloc, s) catch @panic("OOM");
    const res = _parse(options, &it);
    return .{ .iterator = it, .parseResult = res };
}
