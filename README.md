AlbanKnights
============

AlbanKnights is a BSD3 Licensed utility tool, written in Haskell, for Project Belten on MMORPG Mabinogi.

* [Mabinogi](http://mabinogi.nexon.co.jp/)(ja)
* [Project Belten](http://mabinogi.wikiwiki.jp/?%A5%AF%A5%A8%A5%B9%A5%C8%2F%C6%C3%CA%CC%C8%C9)(ja)

## Description
AlbanKnights gives you keywords that the NPCs in Belten like automatically. Even though there is a [Keyword Table](http://mabinogi.wikiwiki.jp/?%BB%E4%A4%CE%B5%B3%BB%CE%C3%C4%B2%F1%CF%C3%A5%C6%A1%BC%A5%D6%A5%EB)(ja) but finding your own index of each NPCs is a little bit bothersome job, isn't it?

And poor Shuan... I think he must take a vacation, so this tool will help him out from messed overwork, even if he can't take his bonus while using this utility though.

At last, there was no special reason to write the code in haskell, just I wanted to learn it (actually, this code is my first challenge to solve RealWorld problem using the language) and let the program executable. Because of it, you can run this program immediately after download and unzip the archive without compiling, building, download implementations and so on.

## Features
* Prompt based console program, from good old days.
* Keep keyword indices of each NPCs: Dai, Eirlys, Kaour and Elsie.
* You can `set`, `unset` or `hold` the index specifying NPC name.
* When you type `next`, the indices will be calculated and re-set.

## Planned Features
- [ ] Suggest index where currently you are.
- [ ] Support special indices 98 and 99.
- [ ] Localize; or remove hard-coded Japanese messages.

## Usage
When you run the program, you'll see a prompt like this:

`AlbanKnights(1): `

Now you're ready to use commands.

### Commands
* `npc [index]` displays keywords for specified NPC.
* `set npc index` sets index of NPC to current program instance.
* `unset npc` clears index of specified NPC from current program instance.
* `list` displays all keywords of NPCs who index is set.
* `next` increments indices by 3 for all NPCs without holded one.
* `hold npc` fixes index of NPC. This index won't be incremented when `next` called.
* `unhold npc` unfixes index of NPC.
* `exit` displays current index of NPCs and exit the program.

All `npc` must be proper form, `dai`, `eirlys`, `kaour` or `elsie`.

### Aliases
There are some aliases of NPCs.

NPC name | Alias1    | Alias2
-------- | --------- | ------
`dai`    |           | `d`
`eirlys` | `airi-su` | `a`
`kaour`  | `kaoru`   | `k`
`elsie`  | `erusii`  | `e`

Also, there are some aliases of commands.

Command | Alias1 | Alias2 | Alias3 | Alias4
------- | ------ | ------ | ------ | ------
`set`   | `s`
`unset` | `u`
`list`  | `ls`   | `l`
`next`  | `x`    | `update` | `up`
`hold`  | `h`    | `unhold` | `lock` | `unlock`
`exit`  | `quit` | `q`

For example, `set eirlys 27` can be replaced with `s a 27`.

### Work-through
1. First, you should `set` index of NPC(s).
1. Then confirm which keywords and index is right using `ls`.
1. *In game*, talk to NPC(s) with displayed keywords.
1. At last, use `x` to update list (before do this, you may use `h` to hold index)
1. If you want to quit the program, type `exit`.
1. Back to 2.

## Install
**[Download latest binary](https://github.com/sandmark/AlbanKnights/releases/latest)**

Currently supported only Windows.

## Contribution
1. Fork it (http://github.com/sandmark/AlbanKnights/fork)
2. Create your feature branch (git checkout -b my-new-feature)
3. Commit your changes (git commit -am 'Add some feature')
4. Push to the branch (git push origin my-new-feature)
5. Create new Pull Request

## Author
[sandmark](https://github.com/sandmark)

## License
[BSD3](LICENSE)

## Special Thanks
[Mabinogi Wiki](http://mabinogi.wikiwiki.jp/)(ja)
