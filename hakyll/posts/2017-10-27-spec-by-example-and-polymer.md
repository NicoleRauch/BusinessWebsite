---
layout: post
author: Nicole Rauch
# meta: Meta Data Goes Here
title: Specification By Example And Polymer
lang: en
---

So, my current client has this nice and shiny Polymer webapp, and now they want to describe and test its behaviour using Specification by Example. No problem, right? Well, let's look at what this actually means, layer by layer.

(Hint: If you are only interested in my solution but not in my decisions, then please feel free to explore [my  example toolchain](https://github.com/NicoleRauch/SpecByExampleAndPolymer)!)


## Specification By Example

One of the core points of Spec by Example is that all tests that are relevant to the business people are actually *readable* by the business people, which I believe is a pretty smart idea. I think FitNesse (old as it may be) together with  Slim does a good job at this, and my client thinks so, too, so let's stick with FitNesse for the moment.

## FitNesse and JavaScript

Luckily, there is a very nice [Node.js Slim Server for FitNesse, called SlimJS](https://github.com/noamtcohen/SlimJS). This allows us to write the glue code (or "fixtures" in FitNesse parlance) in JavaScript (in Node.js, to be precise), so that we can access the production JavaScript code from these fixtures and run it from FitNesse.

## Polymer

In the end of the day, we need to test Polymer components, because we have a Polymer frontend, remember? Well, does it actually matter whether we test Polymer or any other frontend code? Unfortunately, it does, because Polymer makes use of Web Components. This means that a Polymer component consists of a piece of HTML which contains a script tag in which the component's JavaScript code is embedded. In order to get access to this embedded JavaScript code, we need to load the component into a DOM.

## DOM? Wait, wasn't there...

DOMs come in multiple flavours. For once, there is the Real Thing(TM), e.g. a browser like Chrome or Firefox. While this is the most authentic solution, this is actually the slowest, and so maybe there are some alternatives? Well, on the one hand there is PhantomJS, a headless browser which is often preferred because it does not render anything and therefore is much faster than a real browser. But unfortunately, the PhantomJS engine is rather old and therefore not capable of handling bleeding-edge hot new technologies like Web Components. But wait, SlimJS runs its stuff in Node.js, so wasn't there this nice DOM replacement called JSDOM that has become so popular for React.js testing? Indeed, this exists, but it also suffers from old age, and although [I tried really hard](https://github.com/tmpvar/jsdom/issues/1030#issuecomment-320992027), I could not make it play nicely with Polymer components. So, for now I don't see any options to test JavaScript code directly from FitNesse (at least until Polymer 3 with its standalone JavaScript files is released and also picked up by my client). The only thing that's left is our good old friend called "UI-Testing", and that's usually done with Selenium.


## Selenium

There are plenty of Selenium adapters available for FitNesse, so this does not seem to be a problem. Turns out that all of these libraries require the fixtures to be written in Java. Well, my client has the excellent idea that the frontend (JavaScript) devs themselves write the fixtures for their frontend, and many of them don't know Java. Not a big deal, you may think, didn't we just learn about SlimJS? That's true, but remember that we need to connect this with Selenium still.

## Webdriver.IO

Interestingly enough, there exists a Node.js wrapper for Selenium, called [Webdriver.IO](http://webdriver.io/). So why not plug these two together to make FitNesse work with Selenium via JavaScript? Great idea!
Well, until you discover that - of course - each Webdriver call to the browser happens asynchronously, and we have to somehow handle that because the FitNesse fixture methods only work synchronously. Luckily, nowadays we have at least 3 options to handle asynchronicity: Callbacks, Promises, and async/await. Let's see how we can get this to cooperate with SlimJS and FitNesse.

### Callbacks

Callbacks only work in an asynchronous environment. As we don't have this in the SlimJS fixtures because all fixture methods are meant to be synchronous (which is a great design decision if you ask me), we need not even think about using callbacks.

### Promises

Webdriver.IO returns promises from its calls. But because it also implements a fluent interface where multiple queries to a given website can be chained together, these promises are a kind of hybrid beasts. SlimJS, on the other hand, is really strict with the promises it accepts, and it makes sure that they only contain one property, namely `then`. Therefore, for this beautiful (should I say "promising"?) fixture implementation, which is exactly what we need, SlimJS does not resolve the returned promise:

```
this.Title = function () {
  return _browser.url(_url).getTitle();
};
```

### Async/Await

async/await sounds like the next silver bullet when it comes to handling asynchronicity. In order to use it, a function is declared `async` and the asynchronous call inside that function is marked with `await`. The resulting code  looks like this:

```
this.Title =  async function () {
   return await _browser.url(_url).getTitle();
};
```

Unfortunately, this does not work at all in our setting. I did not investigate why this actually fails; maybe FitNesse does not like this, or SlimJS somehow prevents this from working? I'm clueless here.

### Working Around

In order to resolve this problem, we have to provide SlimJS with a Promise-like object that it actually accepts. Therefore, we have to wrap the existing Promise like this:

```
this.Title = function () {
  return {
    then: function (fulfill) {
      _browser.url(_url).getTitle()
      .then(function (title) {
        fulfill(title);
      });
    }
  }
};
```

This way, we give it an object that contains a `then` property and nothing else. And now, finally, we get the desired behaviour! Easy, wasn't it? ;-)


## Putting it all together

We finally end up with the toolchain

FitNesse -> SlimJS -> Webdriver.IO -> Selenium -> UI Testing of Polymer components.

To demonstrate how this works together, I have assembled [an example](https://github.com/NicoleRauch/SpecByExampleAndPolymer) that you can download and explore yourself if you want to make use of the tools presented here.

## What's missing?

We did not talk about where the Polymer webapp is actually run, and where the Selenium server is run. This needs to be integrated into the build / testing pipeline, and for Selenium we might perhaps want to look at Selenium Grid. There are also some Cloud solutions available. Please mix and match as you see fit.
