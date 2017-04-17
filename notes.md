1. Results may differ from shell command versions listed [here][1].

    In `dpkg-query` output along regular md5 hashes there are:

    - `newconffile` entires, which means.. i don't know what.
    - `obsolete` entries, which list, i think, obsolete hashes. For the same
      file there may be another entry with not obsolete hash.
    - several entries for the same file.

    All shell implementations just include extra results due to the same
    config listed twice in `dpkg` db, hashes marked as `obsolete`, etc.

    Difference from `debsums -ce` are only in some files, listed as obsolete
    in some packages. The hash did not match obsolete hash, but matches
    not-obsolete one. Though, `debsums` still reports failure..

2. I first `readEtc` and than load hashes for files, which are present in
   `ConfMap`. Thus:

    - Files in directories, where i haven't permissions to go, will be missed
      from `ConfMap` regardless of whether i have hash for them in `dpkg` db
      or not.
    - Non-existent files, but present in `dpkg`, will be missed.

    Symlinks will be detected properly, because i first read `/etc` and
    determine real file type and then load hash into existing `ConfMap` record
    (the other way, when i first load hashes, i won't know file type and may
    only assume all hashes refer to files).

3.  Some files, which have hashes in `dpkg` db, are actually symlinks. Like
    `/etc/init.d/binfmt-support`.  So i need a complete `FileInfo` structure
    for symlinks. I may implement this using two ways:

    1. Symlink structure represented in constructors:

        data CInfo = FileInfo { .. } | SymLinkInfo {_target :: CInfo}

        Then to reach a `FileInfo` of a file pointed by symlink i'll need to
        traverse entire constructors chain. The possible problem here may be
        from cyclic symlinks. Probably, i need to restrict symlink depth using
        type-level numbers? May i use `uniplate` for traversal?

    2. Symlink is just a property of a file

        `data FileInfo = FileInfo { .. , _target :: [FilePath]}

        Then if several symlinks point to the same file, i'll have several
        `FileInfo`-s for one actual file, but representing different points in
        symlink path.  I.e. there will be overlap. And since here symlink path
        is a list or map, it should be simpler to check depth and cycles.

    Since the primary goal of a program is to work with file hashes, i think,
    the 2nd approach is better for now.

4. If a file has _only_ obsolete hashes and its computed hash does not match
   to them, this file won't be listed with `--changed --stored`, because..
   there is no stored hashes for it. Though, it will be listed with `--changed
   --stored --obsolete`.  This is a feature.

5. The `dpkg` output parsers work on both

        dpkg-query -W -f='${Conffiles}\n'

    and

        dpkg-query -W -f='${Package} ${Status}\n${Conffiles}\n'

    though information about package, to which config file belongs, available
    only in the second case.

6. Composable command-line options:

    - `--stored` and `--obsolete` should choose on which loaded hashes to
      operate.
    - `--changed` should switch default "equal" comparison to "not equal".
    - `--other`, which should list files without loaded hashes, is in fact in
      the same group as `--changed`: it requires _both_ `--stored` and
      `--obsolete` hashes to be an empty list.  I.e. this an operation, like
      "not equal" working on a hash list.

[1]: https://serverfault.com/questions/90400/how-to-check-for-modified-config-files-on-a-debian-system
