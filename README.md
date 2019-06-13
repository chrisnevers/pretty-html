# pretty-html

The purpose of this project is twofold:
1. To write an indentation sensitive parser
2. To provide a prettier alternative to writing HTML.

I use `HTML` in a very rudimentary way, so the grammar for `pretty-html` may not fully encapsulate all of `HTML`'s capabilties.


        Document := [Element]

        Element :=
            | Tag [Attribute] [Element]
            | Content

        Content :=
            | '<string>'

        Attribute :=
            | <string> = '<string>'


The `pretty-html` should be written so that the tag and the attributes are on the same line.

        h1 class='headers'
            'Hello, World!'

If the attributes exceed onto the next line, they should be indented past the tag it belongs to.

        h1 class='headers' id='Title'
            color='blue'
            'Hello, World!'

A child element should be indented more than the tag of the parent element. All of an element's children should be indented at the same length.

For example:

    div class='purple'
        h1 'Hello'
        img src='shrek.png'
        p
            'This is a picture of Shrek'

