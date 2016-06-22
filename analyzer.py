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

        pending_bracket_res = re.compile("([^\(\) \t\n\r]*)\W*\(([^\)\(]*?)$").search(line)
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
                            result += data if data != "" else "--no parameters--"
                        return result
                    else:
                        sub_parts = function_name.split(".")
                        if len(sub_parts) > 1:
                            if sub_parts[0] == "this":
                                sub_parts = sub_parts[1:]
                            
                            sub_index = 0
                            sub_type = sub_parts[0]

                            local_member = self.locate_member(sub_type, content)
                            if len(sub_parts) > 1 and len(local_member) != 0:
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
                                        if sub_item["node"] == "method":
                                            result = ("+" if sub_item["visibility"] == "public" else "*" if sub_item["visibility"] == "protected" else "-") + " " + sub_item["name"] + "(" + sub_item["parameters"] + ")" + ":" + sub_item["type"] + "\n"
                                        else:
                                            result = ("+" if sub_item["visibility"] == "public" else "*" if sub_item["visibility"] == "protected" else "-") + " " + sub_item["name"] + ":" + sub_item["type"] + "\n"
                                            
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

            if previous == "new":
                for data in self.parser.complete_class(current):
                    result += data["package"] + "." + data["name"] + "\n"

                return result

        pending_no_bracket_res = re.compile("([^\(\) \t\n\r]*)$").search(line)
        if pending_no_bracket_res is not None:
            pending_no_bracket_groups = pending_no_bracket_res.groups()
            if len(pending_no_bracket_groups) == 1:
                function_name = pending_no_bracket_groups[0]

                local_function = self.locate_partial_function(function_name, content)
                if len(local_function) != 0:
                    for data in local_function:
                        result += data[0] + "(" + data[1] + ")" + ":" + data[2] + "\n"
                    return result
                else:
                    sub_parts = function_name.split(".")
                    if len(sub_parts) > 1:
                        if sub_parts[0] == "this":
                            sub_parts = sub_parts[1:]
                            
                        local_member = self.locate_member(sub_parts[0], content)
                        if len(sub_parts) > 1 and len(local_member) != 0:
                            result = self.iterate_dotted_string(sub_parts[1:], local_member[0])
                            
                            return result
                        else:
                            return ""
                    else:
                        return ""

        return ""

    def iterate_dotted_string(self, parts, type):
        result = ""

        sub_parts = self.parser.complete_member("^" + parts[0] + ("" if 1 == len(parts) else "$"), type)
        if len(sub_parts) > 0:
            if 1 == len(parts):
                for sub_item in sub_parts:
                    if sub_item["node"] == "method":
                        result += ("+" if sub_item["visibility"] == "public" else "*" if sub_item["visibility"] == "protected" else "-") + " " + sub_item["name"] + "(" + sub_item["parameters"] + ")" + ":" + sub_item["type"] + "\n"
                    elif sub_item["node"] == "getter":
                        result += ("+" if sub_item["visibility"] == "public" else "*" if sub_item["visibility"] == "protected" else "-") + " " + sub_item["name"] + ":" + sub_item["type"] + " <setter>" + "\n"
                    elif sub_item["node"] == "setter":
                        result += ("+" if sub_item["visibility"] == "public" else "*" if sub_item["visibility"] == "protected" else "-") + " " + sub_item["name"] + ":" + sub_item["type"] + " <getter>" + "\n"
                    else:
                        result += ("+" if sub_item["visibility"] == "public" else "*" if sub_item["visibility"] == "protected" else "-") + " " + sub_item["name"] + ":" + sub_item["type"] + "\n"
            else:
                for sub_item in sub_parts:
                    result += self.iterate_dotted_string(parts[1:], sub_item["type"])

        return result

    def locate_member(self, name, content):
        stripped_content = content.replace("\n", "").replace("\r", "")
        
        result = re.compile("var\W+" + name + "\W*:\W*(\w+)").findall(stripped_content)
        if len(result) > 0:
            return result
        else:
            result = re.compile("function\W+get\W+" + name + "\W*\([^\(]*\)\W*:\W*(\w+)").findall(stripped_content)
            if len(result) > 0:
                return result
            else:
                sub_result = re.compile("extends\W+([^ \t\n\r]+)").findall(stripped_content)
                if len(sub_result) > 0:
                    sub_result = self.parser.complete_member("^" + name + "$", sub_result[0])
                    if len(sub_result) > 0:
                        return [sub_result[0]["type"]]
                #result = re.compile("function\W+set\W+" + name + "\W*\([^\(:]*:*([^\(]*)\)\W*:\W*\w+").findall(stripped_content)
                #print result
                #if len(result) > 0:
                #    return result
                #else:
                #    return []

        return []

    def locate_function(self, name, content):
        result = re.compile("function\W+" + name + "\W*\(([^\(]*)\)\W*:\W*(\w+)").findall(content.replace("\n", "").replace("\r", ""))
        if result is not None:
            return result
        else:
            return []

    def locate_partial_function(self, name, content):
        result = re.compile("function\W+(" + name + "[^ \t\n\r\(]*)\W*\(([^\(]*)\)\W*:\W*(\w+)").findall(content.replace("\n", "").replace("\r", ""))
        if result is not None:
            return result
        else:
            return []
