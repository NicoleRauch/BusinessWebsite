---
layout: post
author: Nicole Rauch
# meta: Meta Data Goes Here
title: Lightweight Stubbing of AJAX Requests in JavaScript
subtitle: Fast Feedback As If You Meant It
lang: en
---

AJAX requests are a standard way for a frontend application running in a browser to fetch information from a backend server. 
The interaction between frontend and backend is an important part of those applications, and as always, we want to test it. And, especially if the logic around the AJAX calls is a bit more involved, we want to test this in integration.

Again, as always, we want fast and easy feedback, that means our tests should be easy to run, and they should run in milliseconds. These requirements indicate that we must test our AJAX request without actually executing them against a (real or mock) server.

This article shows various techniques that will give you good and fast test coverage for the AJAX calls your frontend emits.

The testing techniques shown here are independent from the concrete AJAX implementation you're using; no matter whether the requests are emitted with JQuery, NanoAjax or any other library.

## The Example

As a running example for this article, we will develop and test a <code>Username</code> component that accepts a username as input (imagine this will be used on a social media page or the like) and checks against the backend whether this username is already taken (because we require our usernames to be unique throughout the application).
The component shown here is written in React.js, but this is not the point; the techniques presented in this article are suitable for any frontend technology.

This is the initial implementation of our component:

<pre>
export default class Username extends Component {

    constructor(props) {
        super(props);
        this._onChange = this._onChange.bind(this);
    }

    render() {
        return (
            &lt;div>
                &lt;label>Username&lt;/label>
                &lt;input ref="username" onChange={this._onChange}/>
            &lt;/div>
        );
    }

    _onChange(event) {
        // let's call the backend here...
    }
}
</pre>

Basically, it is just a labelled input field that invokes a handler function each time the user types something into the field.

## Dependency Injection

Now let's add the validation step which consists of an AJAX call to the backend.
Of course, we can directly make that call in the handler function, but that would not be very testable.
So, the first step is to wrap the AJAX request into its own little module and to inject that module from the outside. Here, 
we can either inject a validation module via the React.js props, or we fall back to using the real validation module, thus making the injection optional.
This way, you can inject an AJAX request module stub in your <code>Username</code> component test without the need to worry about dealing with AJAX at all. 

The AJAX module takes two parameters, the username that was typed in and a callback that gets invoked when the AJAX call is returned. The call's result is passed to the callback. We want to give feedback to the user whether the username was ok or not. Therefore, we store the result in the component's state and display a warning when the username was already taken.
This is what our fully functional component looks like:

<pre>
export default class Username extends Component {

    constructor(props) {
        super(props);
        this._onChange = this._onChange.bind(this);
        this.validateInBackend = props.validateInBackend || validateInBackend;
        this.state = {isAvailable: true};
    }

    render() {
        const warning = this.state.isAvailable ? "" : "Nickname not available!";
        return (
            &lt;div>
                &lt;label>Username&lt;/label>
                &lt;input ref="username" onChange={this._onChange}/>
                &lt;span ref="warning" style={{color: "red"}}>{warning}&lt;/span>
            &lt;/div>
        );
    }

    _onChange(event) {
        this.validateInBackend(event.target.value, availability => {
            this.setState(availability);
        });
    }
}
</pre>

Now we can test all of the widget's functionality by stubbing out the whole validation module:

<pre>
describe("UsernameTest", function () {

    beforeEach(function () {
        global.document = jsdom.jsdom("&lt;!doctype html>&lt;html>&lt;body>&lt;/body>&lt;/html>");
        global.window = global.document.defaultView;

        this.validateInBackend = sinon.spy();
        this.component = TestUtils.renderIntoDocument(&lt;Username validateInBackend={this.validateInBackend}/>);
    });

    afterEach(function () {
        delete global.document;
        delete global.window;
    });


    it("onChange handler is called when input changes", function () {
        var input = ReactDOM.findDOMNode(this.component.refs.username);
        input.value = "myname";
        TestUtils.Simulate.change(input);

        expect(this.validateInBackend.firstCall.args.length).to.eql(2);
        expect(this.validateInBackend.firstCall.args[0]).to.eql("myname");
        expect(this.validateInBackend.firstCall.args[1]).to.be.a.function();
    });
});
</pre>

We could write this test using Karma, QUnit or other testing frameworks, and we could run it in a browser. 
But as I said above, we like fast tests, and firing up a full-fledged browser is not exactly fast. 
Therefore, we decided to run the tests without any browser -- instead, we use Mocha, a Node.js based test runner. Of course, we still need the DOM, especially the global <code>document</code> and <code>window</code>. We emulate these with the help of jsdom which is a DOM implementation in JavaScript. 

By stubbing out the validation module, we can test all of our component's functionality without ever needing to think about the AJAX call.

## Stubbing an AJAX request

Of course, we also need to test the validation module itself. By the way, this is the module:

<pre>
export default function (username, callback) {
    ajax.ajax("/api/validate?username=" + username, (code, text) => {
        if (code === 200) {
            callback({isAvailable: JSON.parse(text).available});
        } else {
            callback({isAvailable: false});
        }
    });
}
</pre>

For this module, we want to test two aspects: Firstly that it actually makes a call to the backend and uses the correct URL, and secondly that it passes to the callback the data it received from the backend. We do not want to actually perform HTTP calls against a real server because this would require too much infrastructure and would be much too slow, so we make use of [Sinon](http://sinonjs.org)'s
[FakeXMLHttpRequest](http://sinonjs.org/docs/#FakeXMLHttpRequest) stubbing feature instead. This feature allows to capture all HTTP requests that were emitted, to inspect their parameters and to respond to them as desired.

In order to do this, you need to set the variable <code>global.XMLHttpRequest</code> to Sinon's <code>FakeXMLHttpRequest</code> object and implement an <code>onCreate</code> handler function for it that saves all requests to an array for later inspection.

<pre>
describe("validateInBackend", function () {
    beforeEach(function() {
        this.requests = [];
        global.XMLHttpRequest = sinon.FakeXMLHttpRequest;
        global.XMLHttpRequest.onCreate = request => {
            this.requests.push(request);
        };
    });

    afterEach(function () {
        delete global.XMLHttpRequest;
    });

    it("makes request to the backend", function () {
        validateInBackend("myUsername", () => {});

        expect(this.requests.length).to.be(1);
        expect(this.requests[0].url).to.be("/api/validate?username=myUsername");
        expect(this.requests[0].method).to.be("GET");
    });

    it("passes the retrieved data to the callback", function(done) {
        validateInBackend("validUsername", data => {
            expect(data).to.eql({available: true});
            done();
        });

        this.requests[0].respond(200, {"Content-Type": "application/json" }, `{ "available": true }`);
    });
});
</pre>

In the first test, "makes request to the backend", we pass to the module a callback that does not do anything; we do this because we want to fully contentrate on the request. No other requests are triggered in our test (which we explicitly check in the first <code>expect</code>), therefore we can inspect the first request that was stored in our <code>requests</code> array and see whether it used the correct URL.

In the second test, we implement a callback that does the actual validation of the data passed to it. Note here the use of the done() callback to tell Mocha that the test was completed. This is mandatory for asynchronous checking of results. After we have triggered the AJAX call by invoking <code>validateInBackend()</code>, we can again access the first request that we collected, this time responding to it with some stubbed serialised JSON data. The check in the callback body now guarantees that this data was actually passed to the callback after it was parsed to JSON.


## Stubbing multiple AJAX requests

Now let's assume our application's AJAX logic is a bit more challenging. For example, we might want to log to the backend how much time each validation roundtrip took, to get an impression of how long the user needs to wait for the answer. 
We might even want to take this further, for example by triggering another backend call that suggests some alternative usernames in case the desired username was already taken. Implementing and testing this is left as an exercise for the reader.

In these cases, i.e. when emitting new requests after a response came in, the AJAX response stubbing technique shown above is not sufficient because it will only capture the first round of requests, not any subsequent ones. Of course, we could address the subsequent requests after the first ones have been dealt with, but if this happens multiple times, the tests will get rather complicated and - even worse - will eventually match the implementation quite closely, thus being prone to failing when the actual implementation changes. Wouldn't it be nice if we could just set up all answers at the start of the test that we want to give, no matter at which point in time we might need them?

Sinon's [FakeServer](http://sinonjs.org/docs/#fakeServer) allows to do exactly that.

We'll demonstrate this by implementing the abovementioned logging of the roundtrip time.

First of all, let's have a look at our new logging module:

<pre>
export default function (milliseconds, callback) {
    ajax.ajax({url: "/api/log", method: "POST", body: "duration=" + milliseconds}, () => { callback() });
}
</pre>

We'll skip showing the tests here because they are pretty straightforward, and nothing new. Instead, let's put this new module to action. For the sake of brevity, we just invoke the logging in the validation module without injecting it:

<pre>
export default function (username, callback) {

    var start = Date.now();

    ajax.ajax("/api/validate?username=" + username, (code, text) => {
        logTimestamp(Date.now() - start, () => {
            if (code === 200) {
                callback({isAvailable: JSON.parse(text).available});
            } else {
                callback({isAvailable: false});
            }
        });
    });
}
</pre>

We immediately pay the bill for omitting the injection because one of our tests fails! It now needs to provide a response to the second AJAX request as well:

<pre>
describe("validateInBackend", function () {

    // ....

    it("passes the retrieved data to the callback", function(done) {
        validateInBackend("validUsername", data => {
            expect(data).to.eql({isAvailable: true});
            done();
        });

        this.requests[0].respond(200, {"Content-Type": "application/json" }, `{ "available": true }`);
        this.requests[1].respond(200);
    });
});
</pre>

Of course, this is a strong indicator that we should provide a better separation of concerns here, but for the sake of this post I'll leave you to it.

So far, so well; we have tested all modules in isolation, but where are our integration tests? Let's have a look at them.

Firstly, we need to set up the Sinon FakeServer. This is a bit tricky because it does not play well with jsdom if we don't force it to. This forcing is done in the beforeEach: <code>sinon.xhr.supportsCORS = true;</code> makes the FakeServer handle XHR correctly even when run in a jsdom environment. After we've mastered this, we can create the fake server and make it available in the global XMLHttpRequest.

In the tests, we can now stub as many AJAX calls as we need, using the server's respondWith() function. If we are not sure which requests we actually get, we can print them to the console with the line <pre>this.server.respondWith(response => { console.log(response.url); });</pre> (of course we can print as much information about the request as we like).

After we stubbed enough AJAX responses, we can trigger our action and inspect any results we might be interested in. Note that we need not use the done() function in our example because we are testing everything synchronously here. This is made possible by the `{ respondImmediately: true }` parameter that is passed to the FakeServer on creation - which makes the server behave exactly like this.

<pre>
describe("Username end2end test", function () {

    beforeEach(function () {
        global.document = jsdom.jsdom("&lt;!doctype html>&lt;html>&lt;body>&lt;/body>&lt;/html>");
        global.window = global.document.defaultView;

        sinon.xhr.supportsCORS = true; // evil hack to make the fake server handle xhr
        this.server = sinon.fakeServer.create({ respondImmediately: true });
        global.XMLHttpRequest = this.server.xhr;

        this.component = TestUtils.renderIntoDocument(&lt;Username/>);
    });

    afterEach(function () {
        this.server.restore();
        delete global.XMLHttpRequest;
        delete global.document;
        delete global.window;
    });

    it("does not show warning when nickname is valid", function () {
        this.server.respondWith("GET", "/api/validate?username=goodname",
            [200, {"Content-Type": "application/json"}, `{ "available": true }`]
        );
        this.server.respondWith("POST", "/api/log",
            [200, {"Content-Type": "application/json"}, ""]
        );

        var input = ReactDOM.findDOMNode(this.component.refs.username);
        input.value = "goodname";
        TestUtils.Simulate.change(input);

        expect(ReactDOM.findDOMNode(this.component.refs.warning).innerHTML).to.be("");
    });

    it("shows warning when nickname is invalid", function () {
        this.server.respondWith("GET", "/api/validate?username=alreadyTaken",
            [200, {"Content-Type": "application/json"}, `{ "available": false }`]
        );
        this.server.respondWith("POST", "/api/log",
            [200, {"Content-Type": "application/json"}, ""]
        );

        var input = ReactDOM.findDOMNode(this.component.refs.username);
        input.value = "alreadyTaken";
        TestUtils.Simulate.change(input);

        expect(ReactDOM.findDOMNode(this.component.refs.warning).innerHTML).to.be("Nickname not available!");
    });
});
</pre>

## Recap

By now, you have seen a number of different kinds of tests:

* unit tests for the UI component (using a stub instead of the real AJAX module)
* unit tests for the AJAX module (inspecting each AJAX request in isolation)
* integration tests for UI component and multiple AJAX modules (using a fake server that stubs all responses)

Together, they build a strong and fast test harness for the application.

Speaking of fast: All of the tests shown here run in 25 ms on my machine. Needless to say that I'm using this technique at work as well.

## Code

The full code for this article is available [on GitHub](https://github.com/NicoleRauch/StubbingAjaxRequests).

To run it, prepare the project with Grunt (<code>grunt prepare</code> in the top-level directory) and load the index.html in a webserver (sorry, but you cannot simply open it in a browser because of CORS restrictions).

<hr/>

## Comments

(please comment on this article <a href="mailto:info@nicole-rauch.de?Subject=Your blogpost 'Lightweight Stubbing of AJAX Requests in JavaScript'">via email</a>)
