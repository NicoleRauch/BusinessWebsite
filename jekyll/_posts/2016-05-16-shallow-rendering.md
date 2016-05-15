---
layout: post
author: Nicole Rauch
# meta: Meta Data Goes Here
title: It's Shallow Rendering all the Way Down
lang: en
---

Shallow Rendering was introduced to React in 0.13. Its promise is that one can write tests without requiring a DOM. Sounds great! This article takes a closer look at shallow rendering, how it works, what can be tested with it and what cannot.

## First of all, how do we use shallow rendering?

React comes with a bunch of test utilities called ReactTestUtils. One of their great features is shallow rendering. This is how it is done:

Suppose we have a very simple (function) component

<pre>
import React from "react";

export default () => &lt;p>Simple Function Component&lt;/p>
</pre>

You can shallowly render this component e.g. in the <code>beforeEach</code> section:

<pre>
import ReactTestUtils from "react-addons-test-utils";
import React from "react";
import expect from "must";

import SimpleFunctionComponent from "../src/SimpleFunctionComponent";

describe('SimpleFunctionComponent', function () {
  beforeEach(function () {
    var renderer = ReactTestUtils.createRenderer();
    renderer.render(&lt;SimpleFunctionComponent />);
    this.result = renderer.getRenderOutput();
  });
  
  // ...
});
</pre>

## What do we get out of it?

This is what the whole thing looks like:

<pre>
expect(JSON.stringify(this.result)).to.eql('{"type":"p","key":null,"ref":null,"props":{"children":"Simple Function Component"},"_owner":null,"_store":{}}');
</pre>

As you can see, we basically get a JavaScript object that contains all kinds of useful information about what the component will display on the screen when it gets rendered. (By the way, this object looks the same, whether we create a function component or derive it from <code>React.Component</code>.)

We can examine individual parts of this object to make our tests more resilient. E.g. we can ask for the outermost element's type or for its children:

<pre>
expect(result.type).to.be('p');
expect(result.props.children).to.eql('Simple Class Component');
</pre>

### Nested Components 

Of course, our components are not always that simple. We might also have a nested component, e.g.:

<pre>
import React, {Component} from "react";
import SimpleFunctionComponent from "./SimpleFunctionComponent";

export default class extends Component {
  render() {
    return (
      &lt;div>
        &lt;p>Nested Component with&lt;/p>
        &lt;SimpleFunctionComponent />
      &lt;/div>
    );
  }
}
</pre>

We can of course also render it shallowly:

<pre>
import ReactTestUtils from "react-addons-test-utils";
import React from "react";
import expect from "must";

import NestedClassComponent from "../src/NestedClassComponent";

describe('NestedClassComponent', function () {

  beforeEach(function () {
    var renderer = ReactTestUtils.createRenderer();
    renderer.render(&lt;NestedClassComponent />);
    this.result = renderer.getRenderOutput();
  });
  
  // ...
});
</pre>

and we can examine the resulting object:

<pre>
expect(JSON.stringify(this.result)).to.eql(
  '{"type":"div",' +
  '"key":null,' +
  '"ref":null,' +
  '"props":{"children":' +
  '[{"type":"p","key":null,"ref":null,"props":{"children":"Nested Component with"},"_owner":null,"_store":{}},' +
  '{"key":null,"ref":null,"props":{},"_owner":null,"_store":{}}]' +
  '},'+
  '"_owner":null,'+
  '"_store":{}}');
</pre>

but this is not really that helpful any more. We can still ask more specific questions about the parts of this component:

<pre>
expect(this.result.type).to.be('div');
expect(this.result.props.children.length).to.eql(2);
</pre>

and about its children:

<pre>
expect(this.result.props.children[0].type).to.eql('p');
</pre>

and about its children's children:

<pre>
expect(this.result.props.children[0].props.children).to.eql('Nested Component with');
</pre>

Tests that apply this style become really long-winding and fragile because they are tightly coupled to the actual component layout. To improve this situation, many helper libraries have been developed, with [AirBnB's enzyme](http://airbnb.io/enzyme/) being one of the most well-known and most powerful ones. For the sake of simplicity - and because I want to show you the bare metal of shallow rendering - we will not make use of its amazing search abilities in this article. But if you want to apply shallow rendering in your tests, by all means go and have a look at it.

Even when only using plain vanilla shallow rendering, we can still examine interesting aspects. First of all, we can observe that the second child component (the <code>SimpleFunctionComponent</code>) does not reveal anything interesting when it is rendered into a string:

<pre>
expect(JSON.stringify(this.result.props.children[1])).to.eql('{"key":null,"ref":null,"props":{},"_owner":null,"_store":{}}');
</pre>

That's exactly why this method got its name: it only performs *shallow* rendering, i.e. it only renders our own components one level deep. The nested components are not rendered at all. In order to look at the contained components, we can render *them* shallowly, and then *their* contained components, and so on... So it's shallow rendering all the way down.

Still, we can examine *something* about the child components:

<pre>
import SimpleFunctionComponent from "../src/SimpleFunctionComponent";

// ...

expect(this.result.props.children[1].type).to.eql(SimpleFunctionComponent);
</pre>

We can find out which type they have! (And please note that this is not a string comparison but that we check against the real subcomponent definition here.)
This way, we can examine whether our components are constructed correctly. 

### Conditionally Rendered Subcomponents

This even applies to the dynamic aspects of hooking up the subcomponents. E.g. let's look at a component that either includes a subcomponent or it doesn't, depending on some property passed to it:

<pre>
export default class extends Component {
  render() {
    return (
      &lt;div>
        &lt;p>Do we have a nested component?&lt;/p>
        { this.props.showIt ? &lt;SimpleClassComponent /> : null }
      &lt;/div>
    );
  }
}
</pre>

The shallowly rendered object does not contain the subcomponent if <code>false</code> is passed to the component:

<pre>
import NotReallyNestedClassComponent from "../src/NotReallyNestedClassComponent";

describe('NotReallyNestedClassComponent', function () {
  it('checks the result\'s type and contents', function () {
    const renderer = ReactTestUtils.createRenderer();
    renderer.render(&lt;NotReallyNestedClassComponent showIt={false} />);
    const result = renderer.getRenderOutput();

    expect(result.props.children.length).to.eql(2);
    expect(result.props.children[0].type).to.eql('p');

    expect(result.props.children[1]).to.eql(null);
  });
});
</pre>

The component still has two children, and the first one still is a paragraph, but the second one gets rendered as <code>null</code>.

### Passing Props to Subcomponents

Another aspect of the subcomponent we can examine is whether it was passed the correct props. Let's modify our nested component a little bit:

<pre>
import React, {Component} from "react";
import SimpleFunctionComponent from "./SimpleFunctionComponent";

export default class extends Component {
  render() {
    return (
      &lt;div>
        &lt;p>Nested Component passing prop with&lt;/p>
        &lt;SimpleFunctionComponent passedProp={"Yes!"} />
      &lt;/div>
    );
  }
}
</pre>

Now we can ask the shallow rendering result about the subcomponent's props:

<pre>
expect(this.result.props.children[1].props).to.eql({passedProp: "Yes!"});
// or
expect(this.result.props.children[1].props.passedProp).to.eql("Yes!");
</pre>

These are the most interesting static aspects of React components, and we can both test them with shallow rendering, which is great. But what about some more dynamic aspects?
How far does this approach lead us?

### Callbacks

Let's assume we have a component containing a button, and we are passing a handler function to this component via its props:

<pre>
import React, {Component} from "react";

export default ({onButtonClick}) => (
  &lt;div>
    &lt;p>Button Component with&lt;/p>
    &lt;button type="button"
            onClick={onButtonClick}
    >
      Click me!
    &lt;/button>
  &lt;/div>
);
</pre>

Wouldn't it be nice to be able to make sure that this handler function is actually wired correctly? We can indeed do that, but only if we resort to the magical powers of AirBnB's enzyme that I already mentioned above. It comes with its own shallow rendering function which returns the shallow rendering result inside a wrapper object. This wrapper object allows us to actually simulate clicking the button on our shallowly rendered object:

<pre>
import React from "react";
import sinon from "sinon";
import expect from "must";
import { shallow } from "enzyme";

import ButtonComponent from "../src/ButtonComponent";

describe('ButtonComponent', function () {

  it('simulates click events', () => {
    const handleButtonClick = sinon.spy();
    const wrapper = shallow(
      &lt;ButtonComponent onButtonClick={handleButtonClick} />
    );

    wrapper.find('button').simulate('click');

    expect(handleButtonClick.calledOnce).to.equal(true);
  });
});
</pre>

This is not possible (as far as I know) with plain vanilla shallow rendering because the button's <code>onClick</code> prop is not included in the result object.

### Changing Internal State and Internal Props

And there is even more! Let's look at a checkbox component that reflects the checkbox status in the component's internal state:

<pre>
import React, {Component} from "react";

export default class extends Component {
  constructor(props) {
    super(props);
    this.state = {checked: true};
  }

  render() {
    return (
        &lt;input type="checkbox"
               checked={this.state.checked}
               onChange={(e) => { this.setState({checked: !this.state.checked}) }}
        />
    );
  }
}
</pre>

Using enzyme, we can observe that the internal state is modified when the checkbox is clicked:

<pre>
it('can observe state changes', function () {
  const wrapper = shallow(&lt;CheckboxComponentWithState />);

  expect(wrapper.state('checked')).to.eql(true);

  wrapper.simulate('change');

  expect(wrapper.state('checked')).to.eql(false);

  wrapper.simulate('change');

  expect(wrapper.state('checked')).to.eql(true);
});
</pre>

and also that the component's property is changed as well:

<pre>
it('can observe internal property changes', function () {
  const wrapper = shallow(&lt;CheckboxComponentWithState />);

  expect(wrapper.props().checked).to.eql(true);

  wrapper.simulate('change');

  expect(wrapper.props().checked).to.eql(false);

  wrapper.simulate('change');

  expect(wrapper.props().checked).to.eql(true);
});
</pre>

Please note that those checks only work for the root component, not for any children of it. We're only *shallowly* rendering our components, after all.

### Lifecycle Methods

Another interesting category of tests deals with the React lifecycle methods. Most of those methods also get executed with enzyme's shallow rendering. Let's examine this more closely. This component implements many of the lifecycle methods and writes something to the console when they are invoked:

<pre>
import React, {Component} from "react";

export default class extends Component {

  componentWillMount() {
    console.log("Component will mount.");
  }

  componentDidMount() {
    console.log("Component did mount.");
  }

  shouldComponentUpdate() {
    console.log("Should component update?");
    return true;
  }

  componentWillUpdate() {
    console.log("Component will update.");
  }

  componentDidUpdate() {
    console.log("Component did update.");
  }

  constructor(props) {
    super(props);
    this.state = {checked: true};
  }

  render() {
    return (
      <input type="checkbox"
             checked={this.state.checked}
             onChange={(e) => { this.setState({checked: !this.state.checked}) }}
      />
    );
  }

}
</pre>

We use another state-backed checkbox here because the state change triggers a component update. If we display this component on screen, we get this in the console:

<pre>
Component will mount.
Component did mount.
</pre>

And if we click the checkbox, we get this:

<pre>
Should component update?
Component will update.
Component did update.
</pre>

When we shallowly render the component by executing this test:

<pre>
describe('LifecycleComponent', function () {
  it('executes most lifecycle methods', function () {
    console.log('Before rendering the component.');
    const wrapper = shallow(<LifecycleComponent />);
    console.log('After rendering the component.');
    wrapper.simulate('change');
    console.log('After clicking the checkbox.');
  });
});
</pre>

we get the following:

<pre>
Before rendering the component.
Component will mount.
After rendering the component.
Should component update?
Component will update.
Component did update.
After clicking the checkbox.
</pre>

So, as we can see, the only lifecycle method that did not get invoked is <code>componentDidMount</code>, which is understandable since the component was not mounted in the first place. (We are writing DOM-free tests here, remember?)

## What can we not test on it?

There are of course cases that we cannot cover with shallow rendering: 

As soon as we want to test something in integration (more than one level deep), we are at a loss. Also, as you've seen above, observing state and property changes is only possible for the root component. If you really want to test all state and property changes with shallow rendering, you will probably end up with a lot of very fine-grained components. This might be a good thing or a bad thing, I don't want to judge here. You need to find out which level of granularity is good for you to work with.

Do you know of an example where shallow rendering is not sufficient to test your component? If so, I'd love to hear about it!

## Summary

What we can test with shallow rendering:

1. wiring of subcomponents into the component
1. passing of subcomponent props
1. wiring of handler functions that are passed to buttons etc.
1. changes of state of the root component
1. changes of properties of the root component
1. the effects of most of the lifecycle methods

## Repository

If you are interested in playing around with the code and tests that I showed here, you can find them online [in this repository]().