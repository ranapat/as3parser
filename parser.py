import re
import json

class Parser:
    def __init__(self):
        self.collection =[]

    def complete_class(self, name, loose = False):
        result = []
        for class_object in self.collection:
            if loose:
                expression = re.compile("^.*" + name + ".*$")
            else:
                expression = re.compile("^" + name + ".*$")
                
            if expression.search(class_object["name"]) is not None:
                result.append({ "name": class_object["name"], "package": class_object["package"] })
        return result

    def complete_member(self, name, class_name, loose = False, package = None):
        result = []
        for class_object in self.collection:
            if loose:
                expression = re.compile("^.*" + name + ".*$")
            else:
                expression = re.compile("^" + name + ".*$")

            if class_object["name"] == class_name and ((package is not None and class_object["package"] == package) or (package is None)):
                for method in class_object["methods"]:
                    if expression.search(method["name"]) is not None and method["override"] is False:
                        result.append({ "name": method["name"], "visibility": method["visibility"], "type": method["type"], "parameters": method["parameters"], "node": "method" })
                for member in class_object["members"]:
                    if expression.search(member["name"]) is not None:
                        result.append({ "name": member["name"], "visibility": member["visibility"], "type": member["type"], "node": "member" })
                for getter in class_object["getters"]:
                    if expression.search(getter["name"]) is not None and getter["override"] is False:
                        result.append({ "name": getter["name"], "visibility": getter["visibility"], "type": getter["type"], "node": "getter" })

                for setter in class_object["setters"]:
                    if expression.search(setter["name"]) is not None and setter["override"] is False:
                        result.append({ "name": setter["name"], "visibility": setter["visibility"], "type": setter["type"], "node": "setter" })

                if class_object["extends"] != "":
                    result += self.complete_member(name, class_object["extends"], loose, package)

        return result

    def look_for(self, class_name):
        for class_object in self.collection:
            if class_object["name"] == class_name:
                return class_object
                break

    def cache_to(self, file_name):
        with open(file_name, "w") as outfile:
            json.dump(self.collection, outfile)

    def cache_from(self, file_name):
        with open(file_name) as infile:
            self.collection = json.load(infile)
        
    def parse_directory(self, name):
        for root, dirnames, filenames in os.walk(name):
            for filename in fnmatch.filter(filenames, "*.as"):
                self.collection.append(self.parse_file(os.path.join(root, filename)))

    def parse_file(self, name, debug = False):
        f = open(name, "r")

        content = f.read()
        smashed = re.compile("/\*\*.*?\*/").sub("", content.replace("\n", "").replace("\r", ""))

        package_reg_exp = re.compile("package\W*([^ \t\n\r{]*)\W*{(.*)}")
        class_reg_exp = re.compile("([final]*)\W*([public|private|protected|internal]*)\W*([final]*)\Wclass\W([^ \t\n\r{]+)([^{]*){(.*)}")
        class_extends_reg_exp = re.compile("extends\W*([^ \t\n\r]+)\W*(.*)\W*")
        class_implements_reg_exp = re.compile("implements\W*(.+)\W*")
        class_implements_interfaces_reg_exp = re.compile("\W*")
        class_member_variables_reg_exp = re.compile("([public|private|protected]*)\W*var\W+([^ \t\n\r:]+)[ \t\n\r]*:[ \t\n\r]*([^ \t\n\r;]*)")
        class_methods_reg_exp = re.compile("([override]*)\W*([public|private|protected]*)\W*([override]*)\W*function\W+([^ \t\n\r:\(]+)[ \t\n\r]*\(([^/)]*)\):[ \t\n\r]*([^ \t\n\r;]*)")
        class_getters_reg_exp = re.compile("([override]*)\W*([public|private|protected]*)\W*([override]*)\W*function\W+get\W+([^ \t\n\r:\(]+)[ \t\n\r]*\(([^/)]*)\):[ \t\n\r]*([^ \t\n\r;]*)")
        class_setters_reg_exp = re.compile("([override]*)\W*([public|private|protected]*)\W*([override]*)\W*function\W+set\W+([^ \t\n\r:\(]+)[ \t\n\r]*\(([^/)]*)\):[ \t\n\r]*([^ \t\n\r;]*)")
        class_remove_method_bodies_reg_exp = re.compile("{([^{])*?}")

        package_res = package_reg_exp.search(smashed)

        package_name = ""
        class_name = ""
        class_visibility = ""
        class_final = False
        class_extends = ""
        class_implements = ""
        class_constructor_parameters = {}
        class_member_variables = []
        class_methods = []
        class_getters = []
        class_setters = []

        if package_res is not None:
            package_groups = package_res.groups()
            if len(package_groups) == 2:
                package_name = package_groups[0]
        
                if debug: print "package name is", package_name

                class_res = class_reg_exp.search(package_groups[1])
                if class_res is not None:
                    class_groups = class_res.groups()
                    if len(class_groups) == 6:
                        class_name = class_groups[3]
                        class_visibility = class_groups[1] or "internal"
                        class_final = (class_groups[0] or class_groups[2]) == "final"

                        if debug: print "class name is", class_name
                        if debug: print "class visibility is", class_visibility
                        if debug: print "class final is", class_final

                        if debug: print ""

                        class_extends_res = class_extends_reg_exp.search(class_groups[4])
                        if class_extends_res is not None:
                            class_extends_groups = class_extends_res.groups()
                            if len(class_extends_groups) == 2:
                                class_extends = class_extends_groups[0]
    
                                if debug: print "class extends", class_extends, "\n"

                                class_implements_res = class_implements_reg_exp.search(class_extends_groups[1])
                                if class_implements_res is not None:
                                    class_implements_groups = class_implements_res.groups()
                                    if len(class_implements_groups) == 1:
                                        class_implements = class_implements_interfaces_reg_exp.split(class_implements_groups[0])

                                        if debug: print "class implements", class_implements, "\n"
                        else:
                            class_implements_res = class_implements_reg_exp.search(class_groups[4])
                            if class_implements_res is not None:
                                class_implements_groups = class_implements_res.groups()
                                if len(class_implements_groups) == 1:
                                    class_implements = class_implements_interfaces_reg_exp.split(class_implements_groups[0])

                                    if debug: print "class implements", class_implements, "\n"

                        no_method_bodies_scope = class_groups[5]
                        while class_remove_method_bodies_reg_exp.search(no_method_bodies_scope) is not None:
                            no_method_bodies_scope = class_remove_method_bodies_reg_exp.sub("", no_method_bodies_scope)
    
    
                        class_constructor_parameters = re.compile("function " + class_name + "\W*\(([^/)]*)\)").findall(no_method_bodies_scope)
                        if debug: print "class class contructor parameters", class_constructor_parameters, "\n"

                        class_member_variables = [ {"visibility": a or "private", "name": b, "type": c} for a, b, c in  class_member_variables_reg_exp.findall(no_method_bodies_scope)]
                        if debug: print "class memeber variables", class_member_variables, "\n"

                        class_methods = [ {"override": (a or c) == "override", "visibility": b or "private", "name": d, "parameters": e, "type": f} for a, b, c, d, e, f in class_methods_reg_exp.findall(no_method_bodies_scope)]
                        if debug: print "class methods", class_methods, "\n"
                
                        class_getters = [ {"override": (a or c) == "override", "visibility": b or "private", "name": d, "parameters": e, "type": f} for a, b, c, d, e, f in class_getters_reg_exp.findall(no_method_bodies_scope)]
                        if debug: print "class getters", class_getters, "\n"

                        class_setters = [ {"override": (a or c) == "override", "visibility": b or "private", "name": d, "parameters": e, "type": f} for a, b, c, d, e, f in class_setters_reg_exp.findall(no_method_bodies_scope)]
                        if debug: print "class setters", class_setters, "\n"

        return { "package": package_name, "name": class_name, "visibility": class_visibility, "final": class_final, "extends": class_extends, "implements": class_implements, "constructor": class_constructor_parameters, "members": class_member_variables, "methods": class_methods, "getters": class_getters, "setters": class_setters }
