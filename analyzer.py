import os
import re
import parser
import settings

class Analyzer:
    def __init__(self):
        self.settings = settings.Settings()
        self.parser = parser.Parser()
        self.parser.cache_from(self.settings.class_data)
        
    def guess(self, line, content = ""):
        result = ""

        remove_things_in_closed_brackets_reg_exp = re.compile("(\([^\(\)]*\))")
        while remove_things_in_closed_brackets_reg_exp.search(line) is not None:
            line = remove_things_in_closed_brackets_reg_exp.sub("", line)

        pending_bracket_res = re.compile("([^\(\( \t\n\r]*)\W*\(([^\)\(]*?)$").search(line)
        if pending_bracket_res is not None:
            pending_bracket_groups = pending_bracket_res.groups()
            if len(pending_bracket_groups) == 2:
                function_name = pending_bracket_groups[0]
                function_parameters = pending_bracket_groups[1]
                function_at_parameter = len(function_parameters.split(","))

                local_function = self.locate_function(function_name, content)
                if len(local_function) != 0:
                    for data in local_function:
                        result += function_name + "(" + data[0] + ")" + ":" + data[1] + "\n"
                    return result
                else:
                    matching_class = self.parser.look_for(function_name)
                    if matching_class is not None:
                        for data in matching_class["constructor"]:
                            result += data
                        return result
                    else:
                        sub_parts = function_name.split(".")
                        if len(sub_parts) > 1:
                            sub_index = 0
                            sub_type = sub_parts[0]

                            local_member = self.locate_member(sub_type, content)
                            if len(local_member) != 0:
                                sub_items = self.parser.complete_member(sub_parts[sub_index + 1] + "$", local_member[0])
                                if len(sub_items) > 0:
                                    sub_item = sub_items[0]

                                    while sub_index + 1 < len(sub_parts) - 1:
                                        sub_items = self.parser.complete_member(sub_parts[sub_index + 2] + "$", sub_item["type"])
                                        if len(sub_items) > 0:
                                            sub_item = sub_items[0]
                                        else:
                                            sub_item = None
                                        sub_index += 1

                                    if sub_item is not None:
                                        result = ("+" if sub_item["visibility"] == "public" else "*" if sub_item["visibility"] == "protected" else "-") + sub_item["name"] + "(" + sub_item["parameters"] + ")" + ":" + sub_item["type"] + "\n"
                                        
                                return result
                            else:
                                return ""
                        else:
                            return ""

        current_line_res = re.compile("([^ \t\n\r]+)[ |\t|\n|\r]+([^ \t\n\r]+)$").search(line)
        if current_line_res is not None:
            current_line_groups = current_line_res.groups()
            current = current_line_groups[1]
            previous = current_line_groups[0]
            print "Line ::", line, "\n", "Current ::", current, "\n", "Previous ::", previous

            if previous == "new":
                for data in self.parser.complete_class(current):
                    result += data["package"] + "." + data["name"] + "\n"

                return result
            else:
                return ""


        return ""

    def locate_member(self, name, content):
        result = re.compile("var\W+" + name + "\W*:\W*(\w+)").findall(content.replace("\n", "").replace("\r", ""))
        if result is not None:
            return result
        else:
            return []

    def locate_function(self, name, content):
        result = re.compile("function\W+" + name + "\W*\(([^\(]*)\)\W*:\W*(\w+)").findall(content.replace("\n", "").replace("\r", ""))
        if result is not None:
            return result
        else:
            return []

