# servant-auth-and-purescript-pux-example 

Make sure you have `purescript` installed, then install the JS dependencies in 
the purescript directory. The Haskell code has a custom `Setup.hs` that will 
run `pulp` and place the result in an html file.

```
cd purescript
bower install
npm install
cd ..

stack build
stack exec app
```

While you have servant running, you can access the test app at `localhost:3000`.
