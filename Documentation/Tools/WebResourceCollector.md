# Web Resource Collector
iTasks applications are web-applications. Web applications have dynamic parts such as generated HTML or JSON data, and static parts such as images and Javascript.
Some of the Clean modules in iTasks programs need additional static resources to be bundled with the application for it to work.
The `WebResourceCollector` tool is a simple "linker" that during compilation collects and copies all the necessary static web resources used by your application into a single directory. This folder is the name of your application suffixed with `-www` (e.g. `MyApp-www`).
