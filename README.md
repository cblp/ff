# 𝑓𝑓

A distributed note taker and task manager.

## What's the big deal?

1.  𝑓𝑓 synchronizes via any file sync application, like Yandex.Disk,
    Dropbox, or Google Drive.
2.  𝑓𝑓 is available offline and synchronizes without conflicts thanks to
    [CRDT](https://github.com/cblp/crdt).

## Stability

Alpha. Works on my laptop.
𝑓𝑓's own issues are tracked in 𝑓𝑓 since the first day of development.
Please feel free to test it in your environment.

## Install

    $ git clone https://github.com/cblp/ff.git
    $ stack install

## Usage

### Get some unfinished tasks

    $ ff
    457zi9nhn51-12z3opnykw: buy milk

Or, if no tasks

    $ ff
    nothing

### Add new task or note

    $ ff new 'learn Haskell'

Now you can find it in your agenda:

    $ ff
    457zi9nhn51-12z3opnykw: buy milk
    457zifxyxpz-12z3opnykw: learn Haskell

### Mark task done

Marking done is actually moving to archive.

    $ ff done 457zi9nhn51-12z3opnykw
    archived:
      457zi9nhn51-12z3opnykw: buy milk

Archived tasks don't appear in an agenda:

    $ ff
    457zifxyxpz-12z3opnykw: learn Haskell
