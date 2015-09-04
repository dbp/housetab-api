## Setup

Create database:

```
$ psql template1
template1# create database housetab_devel;
template1# create user housetab_user with password '111';
template1# grant all on database housetab_devel to housetab_user;
```

Load schema:

```
psql -U housetab_user housetab_devel < schema.sql
[enter 111 for password]
```

Then build + run (stack.yaml to be added!).
