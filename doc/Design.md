# Design

Tags are hierarchical. e.g. assignee/simon
`tags` file is just a list of tags:

```
assignee/simon
bug
```

The whole filesystem is just a set of read only directories and symlinks.
