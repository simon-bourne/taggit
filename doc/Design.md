# Design

Tags are heirarchical. e.g. assignee/simon
Files and directories can be tagged. They are tagged with a file called `<name>.tags`.
Tags file is just a list of tags:

```
assignee/simon
bug
```

Create, remove, delete and chmod are not allowed.
The whole filesystem is just a set of read only directories and symlinks. Nothing can be created, updated or deleted.
The filesystem is mounted as a passthrough FUSE mount with an additional directory called tags.
Deleting a file from the owner area, deletes it completely.
