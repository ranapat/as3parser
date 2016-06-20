import json

f = open("file1.as", "r")

content = f.read()
#print content
smashed = content.replace("\n", "").replace("\r", "")
#smashed = content
#print smashed


# data = { 'key1': 'value1', 'key': 2  }

# o = open("file1.json", "w")

# json.dump(data, o)

# o1 = open("file2.json", "r")

#print json.load(o1)

print 30 * "-"


import re



package_reg_exp = re.compile("package\W*([^ \t\n\r{]*)\W*{(.*)}")
class_reg_exp = re.compile("([final]*)\W*([public|private|protected|internal]*)\W*([final]*)\Wclass\W([^ \t\n\r{]+)([^{]*){(.*)}")
class_extends_reg_exp = re.compile("extends\W*([^ \t\n\r]+)\W*(.*)\W*")
class_implements_reg_exp = re.compile("implements\W*(.+)\W*")
class_implements_interfaces_reg_exp = re.compile("\W*")
class_member_variables_reg_exp = re.compile("([public|private|protected]*)\W*var\W*([^ \t\n\r:]+)[ \t\n\r]*:[ \t\n\r]*([^ \t\n\r;]*)")
class_methods_reg_exp = re.compile("([override]*)\W*([public|private|protected]*)\W*([override]*)\W*function\W*([^ \t\n\r:\(]+)[ \t\n\r]*\(([^/)]*)\):[ \t\n\r]*([^ \t\n\r;]*)")
class_getters_reg_exp = re.compile("([override]*)\W*([public|private|protected]*)\W*([override]*)\W*function\W*get\W*([^ \t\n\r:\(]+)[ \t\n\r]*\(([^/)]*)\):[ \t\n\r]*([^ \t\n\r;]*)")
class_setters_reg_exp = re.compile("([override]*)\W*([public|private|protected]*)\W*([override]*)\W*function\W*set\W*([^ \t\n\r:\(]+)[ \t\n\r]*\(([^/)]*)\):[ \t\n\r]*([^ \t\n\r;]*)")
class_remove_method_bodies_reg_exp = re.compile("{([^{])*?}")

package_res = package_reg_exp.search(smashed)

if package_res is not None:
    package_groups = package_res.groups()
    if len(package_groups) == 2:
        package_name = package_groups[0]
        
        print "package name is", package_name

        class_res = class_reg_exp.search(package_groups[1])
        if class_res is not None:
            class_groups = class_res.groups()
            if len(class_groups) == 6:
                class_name = class_groups[3]
                class_visibility = class_groups[1] or "internal"
                class_final = (class_groups[0] or class_groups[2]) == "final"

                print "class name is", class_name
                print "class visibility is", class_visibility
                print "class final is", class_final

                print ""

                class_extends_res = class_extends_reg_exp.search(class_groups[4])
                if class_extends_res is not None:
                    class_extends_groups = class_extends_res.groups()
                    if len(class_extends_groups) == 2:
                        class_extends = class_extends_groups[0]

                        print "class extends", class_extends, "\n"

                        class_implements_res = class_implements_reg_exp.search(class_extends_groups[1])
                        if class_implements_res is not None:
                            class_implements_groups = class_implements_res.groups()
                            if len(class_implements_groups) == 1:
                                class_implements = class_implements_interfaces_reg_exp.split(class_implements_groups[0])

                                print "class implements", class_implements, "\n"
                else:
                    class_implements_res = class_implements_reg_exp.search(class_groups[4])
                    if class_implements_res is not None:
                        class_implements_groups = class_implements_res.groups()
                        if len(class_implements_groups) == 1:
                            class_implements = class_implements_interfaces_reg_exp.split(class_implements_groups[0])

                            print "class implements", class_implements, "\n"

                no_method_bodies_scope = class_groups[5]
                while class_remove_method_bodies_reg_exp.search(no_method_bodies_scope) is not None:
                    no_method_bodies_scope = class_remove_method_bodies_reg_exp.sub("", no_method_bodies_scope)

                class_member_variables = [ {"visibility": a or "private", "name": b, "type": c} for a, b, c in  class_member_variables_reg_exp.findall(no_method_bodies_scope)]
                print "class memeber variables", class_member_variables, "\n"

                class_methods = [ {"override": (a or c) == "override", "visibility": b or "private", "name": d, "parameters": e, "type": f} for a, b, c, d, e, f in class_methods_reg_exp.findall(no_method_bodies_scope)]
                print "class methods", class_methods, "\n"
                
                class_getters = [ {"override": (a or c) == "override", "visibility": b or "private", "name": d, "parameters": e, "type": f} for a, b, c, d, e, f in class_getters_reg_exp.findall(no_method_bodies_scope)]
                print "class getters", class_getters, "\n"


                class_setters = [ {"override": (a or c) == "override", "visibility": b or "private", "name": d, "parameters": e, "type": f} for a, b, c, d, e, f in class_setters_reg_exp.findall(no_method_bodies_scope)]
                print "class setters", class_setters, "\n"

