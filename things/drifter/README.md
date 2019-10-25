# Drifter

An ~endless~ runner

![drifter](https://img.itch.zone/aW1nLzI2MDQ2ODgucG5n/original/5Wjzdd.png)
[https://sendai.itch.io/drifter](https://sendai.itch.io/drifter)

## Run from release Binary

To run the release bin.zip
```
unzip -e bin.zip
make run
```

## Run from Source

```
> (ql:quickload :incandescent/drifter)
> (play-render :start)
```

### Depedencies not in quicklisp

* [https://github.com/azimut/rocketman](https://github.com/borodust/bodge-ode/)
* [https://github.com/azimut/cloud/](https://github.com/borodust/bodge-ode/)
* [https://github.com/azimut/csound](https://github.com/borodust/bodge-ode/)
* [https://github.com/byulparan/scheduler](https://github.com/borodust/bodge-ode/)
* [https://github.com/ormf/cm](https://github.com/borodust/bodge-ode/)
* [https://github.com/cbaggers/cepl.fond](https://github.com/borodust/bodge-ode/)
* [https://github.com/borodust/bodge-ode/](https://github.com/borodust/bodge-ode/)

Also have `csound` and `assimp` library installed from your package manager.
