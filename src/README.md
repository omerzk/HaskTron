# HaskTron

HaskTron is an implementation of tron in Haskell using Helm as it's game engine .

  - Functional reactive programming imatating elm's style
  - Simple game design open to massive extension
  -


currently I'm dealing with a few issues holding this game back:

> an abnoraml amount of cpu(80%+ on the main game screen) usage, most likely stemming > from the engine itself.
> 
> dynamically linked lib's/dependencies prevent the game from running/compiling on > > most machines 


### Version
0.0.0

### Tech

HaskTron uses the following open source projects to work properly:

* [Helm] - A functionally reactive game engine

### Installation

```sh
$ git clone [git-repo-url] HaskTron
$ cd HaskTron
$ npm i -d
$ mkdir -p public/files/{md,html,pdf}
$ gulp build --prod
$ NODE_ENV=production node app
```


### Development

Want to contribute? Great!

Dillinger uses Gulp + Webpack for fast developing.
Make a change in your file and instantanously see your updates!

Open your favorite Terminal and run these commands.


### Todo's

- web multiplayer
- debug cpu usage
- smarter rendering
- decide ,clean-up excess code


License
----

Mine!


**Free Software, Hell Yeah!**

[Helm]:http://helm-engine.org/
