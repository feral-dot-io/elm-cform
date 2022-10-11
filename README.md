# An Elm form library

Use `elm-doc-preview`. Not yet ready to be published. Work in progress.

## TODO

High-level TODO:
- Validation / errors.
- Using in unstructured ways.

More detailed notes on next steps:
- Attributes to control field HTML.
- Review MDN docs for more attributes on controls.
- Fields should get an optional ID. This ID should then be used on the field and as a DB state key. This should avoid DB reusing state with dynamic fields.
- Key needs to be `List String` with the first element being our ID (part of solving dynamic fields).
- We should be able to retrieve a field by its ID.
- We should be able to iterate over controls in a field. This would be useful for unbundling our structured forms and using in other contexts. For example checkboxes in table rows to select items.
- We should be able to control text inputs by transforming events. For example a credit card input could reject non-numeric inputs.
- We need to be able to validate just before we call the setter. This should be able to validate not just the field but use data from the whole output.
- Stylesheets is policy not mechanism.
- An example showcase.
