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
psql -U housetab_user -h localhost -W housetab_devel < schema.sql
Password for user housetab_user: [enter 111]
```

Build:

```
stack build
```

Run:

```
stack exec housetab
```

Then visit it on port 8000. Maybe we should make a way to sign up!
