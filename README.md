# stashh

A CLI client for Atlassian Stash.

## Usage

    The stashh program

    stashh [COMMAND] ... [OPTIONS]

    Common flags:
         --url=ITEM         The stash api url (e.g https://stash.atlassian.com)
         --user=ITEM        Your username on stash
         --password=ITEM    Your password on stash.(for Basic-auth...)
         --start=INT        which item should be used as the first item in the
                            page of results
         --limit=INT        how many results to return per page
      -d --debug            print debug info
      -? --help             Display help message
      -V --version          Print version information

    stashh projects [OPTIONS]

         --name=ITEM        project name
      -p --permission=ITEM  project permission

    stashh repos [OPTIONS] PROJECT_KEY

    stashh pullrequests [OPTIONS] PROJECT_KEY REPOSITORY_SLUG

         --direction=ITEM   (optional,  defaults to INCOMING) the direction
                            relative to the specified repository. Either INCOMING
                            or OUTGOING
         --at=ITEM          (optional) a specific branch to find pull requests to
                            or from.
         --state=ITEM       (optional,  defaults to OPEN) only pull requests in
                            the specified state will be returned. Either OPEN,
                            DECLINED or MERGED.
         --order=ITEM       (optional) the order to return pull requests in,
                            either OLDEST (as in: 'oldest first') or NEWEST.

## Configuration

stashh load default settings from `.stashh` file.
This configuration file is searched from current dirctory to pararents, or your home directory(~/.stashh).
It's contents is like bellow

    url      = "http://your.stash.com/"
    user     = "your_username"
    password = "your basic auth password"

## Contributing

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request
