# Recon Remote Injector/Purge
===============

Using [recon](https://github.com/ferd/recon) usually infers having it on the runtime node.
This might not always be the case in existing projects, where recon is not being included.
With this useful utility you can remotely load/purge recon from a erlang node ( at your own risk! )

## Loading it:
```
./recon_injector inject mynode@mymachine mycookie
```

## Purging:
```
./recon_injector purge mynode@mymachine mycookie
```
