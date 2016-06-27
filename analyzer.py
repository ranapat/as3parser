import os
import re

class Analyzer:
    def __init__(self, parser):
        self.parser = parser

        self.stripped_content = ""

        self.result = []
        
    def guess(self, line, content = ""):
        self.reset_result()
        self.strip_content(content)

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

                local_function = self.locate_function(function_name)
                if len(local_function) != 0:
                    for data in local_function:
                        self.to_result(function_name + "(" + data[0] + ")" + ":" + data[1], data[0])
                    return self.result_to_string("tooltip")
                else:
                    matching_class = self.parser.look_for(function_name)
                    if matching_class is not None:
                        for data in matching_class["constructor"]:
                            if data != "":
                                self.to_result(matching_class["name"] + "(" + data + ")", data)
                            else:
                                self.to_result(matching_class["name"] + "()", "")
                        return self.result_to_string("tooptip")
                    else:
                        sub_parts = function_name.split(".")
                        if len(sub_parts) > 1:
                            if sub_parts[0] == "this":
                                sub_parts = sub_parts[1:]
                            
                            sub_index = 0
                            sub_type = sub_parts[0]

                            local_member = self.locate_member(sub_type)
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
                                            self.to_result(("+" if sub_item["visibility"] == "public" else "*" if sub_item["visibility"] == "protected" else "-") + " " + sub_item["name"] + "(" + sub_item["parameters"] + ")" + ":" + sub_item["type"], sub_item["parameters"])
                                        else:
                                            self.to_result(("+" if sub_item["visibility"] == "public" else "*" if sub_item["visibility"] == "protected" else "-") + " " + sub_item["name"] + ":" + sub_item["type"], "")
                                return self.result_to_string("tooltip")
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
                    self.to_result(data["package"] + "." + data["name"], data["name"].replace(current, ""))
                return self.result_to_string("complete")

        pending_no_bracket_res = re.compile("([^\(\) \t\n\r]*)$").search(line)
        if pending_no_bracket_res is not None:
            pending_no_bracket_groups = pending_no_bracket_res.groups()
            if len(pending_no_bracket_groups) == 1:
                function_name = pending_no_bracket_groups[0]

                local_function = self.locate_partial_function(function_name)
                if len(local_function) != 0:
                    for data in local_function:
                        self.to_result(data[0] + "(" + data[1] + ")" + ":" + data[2], data[0].replace(function_name, ""))
                    return self.result_to_string("complete")
                else:
                    sub_parts = function_name.split(".")
                    if len(sub_parts) > 0:
                        if sub_parts[0] == "this":
                            sub_parts = sub_parts[1:]

                        if len(sub_parts) > 0:
                            local_member = self.locate_member(sub_parts[0])
                            if len(local_member) != 0:
                                self.iterate_dotted_string(sub_parts[1:], local_member[0], sub_parts[len(sub_parts) - 1])
                                return self.result_to_string("complete")
                            else:
                                self.locate_partial_member(sub_parts[0], sub_parts[len(sub_parts) - 1])
                                return self.result_to_string("complete")
                        else:
                            return ""
                    else:
                        return ""

        return ""

    def reset_result(self):
        self.result = []

    def to_result(self, see, write):
        self.result.append( {"see": see, "write": write}  )

    def result_to_string(self, type):
        result = type + "\n"
        for data in self.result:
            result += data["see"] + "@@@" + data["write"] + "\n"
        return result

    def strip_content(self, content):
        self.stripped_content = content.replace("\n", "").replace("\r", "")

    def iterate_dotted_string(self, parts, type, current):
        result = ""

        sub_parts = self.parser.complete_member("^" + parts[0] + ("" if 1 == len(parts) else "$"), type)
        if len(sub_parts) > 0:
            if 1 == len(parts):
                for sub_item in sub_parts:
                    if sub_item["node"] == "method":
                        self.to_result(("+" if sub_item["visibility"] == "public" else "*" if sub_item["visibility"] == "protected" else "-") + " " + sub_item["name"] + "(" + sub_item["parameters"] + ")" + ":" + sub_item["type"], sub_item["name"].replace(current, ""))
                    elif sub_item["node"] == "getter":
                        self.to_result(("+" if sub_item["visibility"] == "public" else "*" if sub_item["visibility"] == "protected" else "-") + " " + sub_item["name"] + ":" + sub_item["type"] + " <setter>", sub_item["name"].replace(current, ""))
                    elif sub_item["node"] == "setter":
                        self.to_result(("+" if sub_item["visibility"] == "public" else "*" if sub_item["visibility"] == "protected" else "-") + " " + sub_item["name"] + ":" + sub_item["type"] + " <getter>", sub_item["name"].replace(current, ""))
                    else:
                        self.to_result(("+" if sub_item["visibility"] == "public" else "*" if sub_item["visibility"] == "protected" else "-") + " " + sub_item["name"] + ":" + sub_item["type"], sub_item["name"].replace(current, ""))
            else:
                for sub_item in sub_parts:
                    self.iterate_dotted_string(parts[1:], sub_item["type"], current)

        return result

    def locate_member(self, name):
        result = re.compile("function\W+" + name + "\W*\([^\(]*\)\W*:\W*(\w+)").findall(self.stripped_content)
        if len(result) > 0:
            return result
        else:
            result = re.compile("var\W+" + name + "\W*:\W*(\w+)").findall(self.stripped_content)
            if len(result) > 0:
                return result
            else:
                result = re.compile("function\W+get\W+" + name + "\W*\([^\(]*\)\W*:\W*(\w+)").findall(self.stripped_content)
                if len(result) > 0:
                    return result
                else:
                    sub_result = re.compile("class\W+[^ \t\n\r]+\W+extends\W+([^ \t\n\r]+)").findall(self.stripped_content)
                    if len(sub_result) > 0:
                        sub_result = self.parser.complete_member("^" + name + "$", sub_result[0])
                        if len(sub_result) > 0:
                            return [sub_result[0]["type"]]
                    #result = re.compile("function\W+set\W+" + name + "\W*\([^\(:]*:*([^\(]*)\)\W*:\W*\w+").findall(self.stripped_content)
                    #print result
                    #if len(result) > 0:
                    #    return result
                    #else:
                    #    return []

        return []

    def locate_partial_member(self, name, current):
        result = re.compile("function\W+(" + name + "[^ \t\n\r]*)\W*\(([^\(]*)\)\W*:\W*(\w+)").findall(self.stripped_content)
        if len(result) > 0:
            for data in result:
                self.to_result(data[0] + "(" + data[1] + ")" + ":" + data[2], data[0].replace(current, ""))
        else:
            result = re.compile("var\W+(" + name + "[^ \t\n\r]*)\W*:\W*(\w+)").findall(self.stripped_content)
            if len(result) > 0:
                for data in result:
                    self.to_result(data[0] + ":" + data[1], data[0].replace(current, ""))
            else:
                result = re.compile("function\W+get\W+(" + name + "[^ \t\n\r]+)\W*\([^\(]*\)\W*:\W*(\w+)").findall(self.stripped_content)
                if len(result) > 0:
                    for data in result:
                        self.to_result(data[0] + ":" + data[1] + " <getter>", data[0].replace(current, ""))
                else:
                    result = re.compile("function\W+set\W+(" + name + "[^ \t\n\r]*)\W*\([^\(:]*:*([^\(]*)\)\W*:\W*\w+").findall(self.stripped_content)
                    if len(result) > 0:
                        for data in result:
                            self.to_result(data[0] + ":" + data[1] + " <setter>", data[0].replace(current, ""))
                    else:
                        sub_result = re.compile("class\W+[^ \t\n\r]+\W+extends\W+([^ \t\n\r]+)").findall(self.stripped_content)
                        if len(sub_result) > 0:
                            sub_result = self.parser.complete_member("^" + name, sub_result[0])
                            if len(sub_result) > 0:
                                for sub_item in sub_result:
                                    if sub_item["node"] == "method":
                                        self.to_result(("+" if sub_item["visibility"] == "public" else "*" if sub_item["visibility"] == "protected" else "-") + " " + sub_item["name"] + "(" + sub_item["parameters"] + ")" + ":" + sub_item["type"], sub_item["name"].replace(current, ""))
                                    elif sub_item["node"] == "getter":
                                        self.to_result(("+" if sub_item["visibility"] == "public" else "*" if sub_item["visibility"] == "protected" else "-") + " " + sub_item["name"] + ":" + sub_item["type"] + " <setter>", sub_item["name"].replace(current, ""))
                                    elif sub_item["node"] == "setter":
                                        self.to_result(("+" if sub_item["visibility"] == "public" else "*" if sub_item["visibility"] == "protected" else "-") + " " + sub_item["name"] + ":" + sub_item["type"] + " <getter>", sub_item["name"].replace(current, ""))
                                    else:
                                        self.to_result(("+" if sub_item["visibility"] == "public" else "*" if sub_item["visibility"] == "protected" else "-") + " " + sub_item["name"] + ":" + sub_item["type"], sub_item["name"].replace(current, ""))

    def locate_function(self, name):
        result = re.compile("function\W+" + name + "\W*\(([^\(]*)\)\W*:\W*(\w+)").findall(self.stripped_content)
        if result is not None:
            return result
        else:
            return []

    def locate_partial_function(self, name):
        result = re.compile("function\W+(" + name + "[^ \t\n\r\(]*)\W*\(([^\(]*)\)\W*:\W*(\w+)").findall(self.stripped_content)
        if result is not None:
            return result
        else:
            return []
