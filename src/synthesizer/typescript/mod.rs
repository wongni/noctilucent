use crate::code::{CodeBuffer, IndentOptions};
use crate::ir::conditions::ConditionIr;
use crate::ir::constructor::ConstructorParameter;
use crate::ir::mappings::{MappingInstruction, OutputType};
use crate::ir::outputs::OutputInstruction;
use crate::ir::conditions::ConditionInstruction;
use crate::ir::reference::{Origin, PseudoParameter, Reference};
use crate::ir::resources::{ResourceInstruction, ResourceIr};
use crate::ir::CloudformationProgramIr;
use crate::parser::lookup_table::MappingInnerValue;
use indexmap::IndexMap;
use std::borrow::Cow;
use std::collections::HashMap;
use std::io;
use std::rc::Rc;
use voca_rs::case::{camel_case, pascal_case};

use super::Synthesizer;

const INDENT: Cow<'static, str> = Cow::Borrowed("    ");

pub struct Typescript {
    // TODO: Put options in here for different outputs in typescript
}

impl Typescript {
    #[cfg_attr(coverage_nightly, no_coverage)]
    #[deprecated(note = "Prefer using the Synthesizer API instead")]
    pub fn output(ir: CloudformationProgramIr) -> String {
        let mut output = Vec::new();
        Typescript {}.synthesize(ir, &mut output).unwrap();
        String::from_utf8(output).unwrap()
    }
}

impl Synthesizer for Typescript {
    fn synthesize(
        &self,
        ir: CloudformationProgramIr,
        output: &mut dyn io::Write,
    ) -> io::Result<()> {
        let code = CodeBuffer::default();

        let imports = code.section(true);
        for import in &ir.imports {
            imports.line(format!(
                "import * as {} from '{}';",
                import.name,
                import.path.join("/"),
            ));
        }

        let context = &mut TypescriptContext::with_imports(imports);

        let iface_props = code.indent_with_options(IndentOptions {
            indent: INDENT,
            leading: Some("export interface NoctStackProps extends cdk.StackProps {".into()),
            trailing: Some("}".into()),
            trailing_newline: true,
        });
        let default_props = {
            let mut default_props: HashMap<&str, String> =
                HashMap::with_capacity(ir.constructor.inputs.len());
            for param in &ir.constructor.inputs {
                let comment = iface_props.tsdoc();
                if let Some(description) = &param.description {
                    comment.line(description.to_owned());
                }
                let question_mark_token = match &param.default_value {
                    None => "",
                    Some(value) => {
                        let value = match param.constructor_type.as_str() {
                            "String" => format!("{value:?}"),
                            _ => value.clone(),
                        };
                        comment.line(format!("@default {value}"));
                        default_props.insert(&param.name, value);
                        "?"
                    }
                };
                iface_props.line(format!(
                    "readonly {}{question_mark_token}: {};",
                    pretty_name(&param.name),
                    pretty_name(&param.constructor_type),
                ));
            }
            default_props
        };
        code.newline();

        if let Some(description) = &ir.description {
            let comment = code.tsdoc();
            comment.line(description.to_owned());
        }
        let class = code.indent_with_options(IndentOptions {
            indent: INDENT,
            leading: Some("export class NoctStack extends cdk.Stack {".into()),
            trailing: Some("}".into()),
            trailing_newline: true,
        });
        if !ir.outputs.is_empty() {
            for op in &ir.outputs {
                if let Some(description) = &op.description {
                    let comment = class.tsdoc();
                    comment.line(description.to_owned());
                }
                // NOTE: the property type can be inferred by the compiler...
                class.line(format!(
                    "public readonly {name};",
                    name = pretty_name(&op.name),
                ));
            }
            class.newline();
        }

        let default_empty = if ir
            .constructor
            .inputs
            .iter()
            .all(|param| param.default_value.is_some())
        {
            " = {}"
        } else {
            ""
        };

        let  ctor = class.indent_with_options(IndentOptions{
            indent: INDENT,
            leading: Some(format!("public constructor(scope: cdk.App, id: string, props: NoctStackProps{default_empty}) {{").into()),
            trailing: Some("}".into()),
            trailing_newline: true,
        });
        ctor.line("super(scope, id, props);");

        if !default_props.is_empty() {
            ctor.newline();
            ctor.line("// Applying default props");
            let obj = ctor.indent_with_options(IndentOptions {
                indent: INDENT,
                leading: Some("props = {".into()),
                trailing: Some("};".into()),
                trailing_newline: true,
            });
            obj.line("...props,");
            for (name, value) in default_props {
                obj.line(format!("{name}: props.{name} ?? {value},"));
            }
        }

        emit_parameters(&ctor, &ir.constructor.inputs);

        emit_mappings(&ctor, &ir.mappings);

        if !ir.conditions.is_empty() {
            ctor.newline();
            ctor.line("// Conditions");

            for cond in &ir.conditions {
                let synthed = synthesize_condition_recursive(&cond.value);
                ctor.line(format!("const {} = {};", pretty_name(&cond.name), synthed));
            }
        }

        emit_conditions(&ctor, &ir.conditions);

        ctor.newline();
        ctor.line("// Resources");

        let mut is_first_resource = true;
        for reference in &ir.resources {
            if is_first_resource {
                is_first_resource = false;
            } else {
                ctor.newline();
            }
            emit_resource(context, &ctor, reference);
        }

        if !ir.outputs.is_empty() {
            ctor.newline();
            ctor.line("// Outputs");

            for op in &ir.outputs {
                let var_name = pretty_name(&op.name);
                let cond = op.condition.as_ref().map(|s| pretty_name(s));
                ctor.text(format!("this.{var_name} = "));
                emit_resource_ir(context, &ctor, &op.value, Some(";\n"), false, false);

                if let Some(export) = &op.export {
                    emit_cfn_output(context, &ctor, op, export, &var_name, cond);
                }
            }
        }

        code.write(output)
    }
}

struct TypescriptContext {
    imports: Rc<CodeBuffer>,
    imports_buffer: bool,
}
impl TypescriptContext {
    const fn with_imports(imports: Rc<CodeBuffer>) -> Self {
        Self {
            imports,
            imports_buffer: false,
        }
    }

    fn import_buffer(&mut self) {
        if self.imports_buffer {
            return;
        }
        self.imports.line("import { Buffer } from 'buffer';");
        self.imports_buffer = true;
    }
}

impl Reference {
    fn to_typescript(&self) -> Cow<'static, str> {
        match &self.origin {
            Origin::Parameter => format!("props.{}", camel_case(&self.name)).into(),
            Origin::LogicalId { conditional } => format!(
                "{var}{chain}ref",
                var = camel_case(&self.name),
                chain = if *conditional { "?." } else { "." }
            )
            .into(),
            Origin::Condition => camel_case(&self.name).into(),
            Origin::PseudoParameter(x) => match x {
                PseudoParameter::Partition => "this.partition".into(),
                PseudoParameter::Region => "this.region".into(),
                PseudoParameter::StackId => "this.stackId".into(),
                PseudoParameter::StackName => "this.stackName".into(),
                PseudoParameter::URLSuffix => "this.urlSuffix".into(),
                PseudoParameter::AccountId => "this.account".into(),
                PseudoParameter::NotificationArns => "this.notificationArns".into(),
            },
            Origin::GetAttribute {
                conditional,
                attribute,
            } => format!(
                "{var_name}{chain}attr{name}",
                var_name = camel_case(&self.name),
                chain = if *conditional { "?." } else { "." },
                name = pascal_case(attribute)
            )
            .into(),
        }
    }
}

fn emit_cfn_output(
    context: &mut TypescriptContext,
    output: &CodeBuffer,
    op: &OutputInstruction,
    export: &ResourceIr,
    var_name: &str,
    cond: Option<String>,
) {
    let output = output.indent_with_options(IndentOptions {
        indent: INDENT,
        leading: Some(format!("new cdk.CfnOutput(this, '{}', {{", &op.name).into()),
        trailing: Some("});".into()),
        trailing_newline: true,
    });

    if let Some(description) = &op.description {
        output.line(format!("description: '{}',", description.escape_debug()));
    }
    output.text("exportName: ");
    emit_resource_ir(context, &output, export, Some(",\n"), false, false);
    output.line(format!("value: this.{var_name},"));
    if cond.is_some() {
        output.line(format!("condition: {}Condition,", cond.as_ref().unwrap()));
    }
}

fn emit_resource(
    context: &mut TypescriptContext,
    output: &CodeBuffer,
    reference: &ResourceInstruction,
) {
    let var_name = pretty_name(&reference.name);
    let service = reference.resource_type.service().to_lowercase();

    append_references(output, reference);

    output.line(format!(
        "const {var_name} = new {service}.Cfn{rtype}(this, '{}', {{",
        reference.name.escape_debug(),
        rtype = reference.resource_type.type_name(),
    ));
    emit_resource_props(context, output.indent(INDENT), &reference.properties);
    output.line("});");

    if let Some(cond) = &reference.condition {
        output.line(format!(
            "{var_name}.cfnOptions.condition = {cond}Condition;",
            cond = pretty_name(cond)
        ));
    }

    emit_resource_attributes(context, output, reference, &var_name);
}

fn emit_resource_attributes(
    context: &mut TypescriptContext,
    output: &CodeBuffer,
    reference: &ResourceInstruction,
    var_name: &str,
) {
    if let Some(metadata) = &reference.metadata {
        let md = output.indent_with_options(IndentOptions {
            indent: INDENT,
            leading: Some(format!("{var_name}.cfnOptions.metadata = {{").into()),
            trailing: Some("};".into()),
            trailing_newline: true,
        });
        emit_resource_metadata(context, md, metadata);
    }

    if let Some(update_policy) = &reference.update_policy {
        output.text(format!("{var_name}.cfnOptions.updatePolicy = "));
        emit_resource_ir(context, output, update_policy, Some(";"), false, false);
    }

    if let Some(deletion_policy) = &reference.deletion_policy {
        output.line(format!(
            "{var_name}.cfnOptions.deletionPolicy = cdk.CfnDeletionPolicy.{deletion_policy};"
        ));
    }

    if !reference.dependencies.is_empty() {
        for dependency in &reference.dependencies {
            output.line(format!(
                "{var_name}.addDependency({});",
                pretty_name(dependency)
            ));
        }
    }
}

fn emit_resource_metadata(
    context: &mut TypescriptContext,
    output: Rc<CodeBuffer>,
    metadata: &ResourceIr,
) {
    match metadata {
        ResourceIr::Object(_, entries) => {
            for (name, value) in entries {
                output.text(format!("'{name}': "));
                emit_resource_ir(context, &output, value, Some(",\n"), true, false);
            }
        }
        unsupported => output.line(format!("/* {unsupported:?} */")),
    }
}

fn emit_resource_props<S>(
    context: &mut TypescriptContext,
    output: Rc<CodeBuffer>,
    props: &IndexMap<String, ResourceIr, S>,
) {
    for (name, prop) in props {
        let prop_name = pretty_name(name);
        output.text(format!("{}: ", prop_name));
        let keep_prop_name = 
            prop_name.to_lowercase() == "assumerolepolicydocument" || 
            prop_name.to_lowercase() == "policydocument";
        // name property is usually a string not a resolvable 
        let use_tenary_op = prop_name.to_lowercase().ends_with("name");
        emit_resource_ir(context, &output, prop, Some(",\n"), keep_prop_name, use_tenary_op);
    }
}

fn emit_resource_ir(
    context: &mut TypescriptContext,
    output: &CodeBuffer,
    value: &ResourceIr,
    trailer: Option<&str>,
    keep_prop_name: bool,
    use_tenary_op: bool,
) {
    match value {
        // Literal values
        ResourceIr::Null => output.text("undefined"),
        ResourceIr::Bool(bool) => output.text(bool.to_string()),
        ResourceIr::Double(float) => output.text(format!("{float}")),
        ResourceIr::Number(int) => output.text(int.to_string()),
        ResourceIr::String(str) => output.text(format!("'{}'", str.escape_debug())),

        // Collection values
        ResourceIr::Array(_, array) => {
            let arr = output.indent_with_options(IndentOptions {
                indent: INDENT,
                leading: Some("[".into()),
                trailing: Some("]".into()),
                trailing_newline: false,
            });
            for item in array {
                emit_resource_ir(context, &arr, item, Some(",\n"), keep_prop_name, use_tenary_op);
            }
        }
        ResourceIr::Object(_, entries) => {
            let obj = output.indent_with_options(IndentOptions {
                indent: INDENT,
                leading: Some("{".into()),
                trailing: Some("}".into()),
                trailing_newline: false,
            });
            for (name, value) in entries {
                if keep_prop_name { 
                    obj.text(format!("'{}': ", name));
                } else {
                    obj.text(format!("'{}': ", pretty_name(name)));
                }
                emit_resource_ir(context, &obj, value, Some(",\n"), keep_prop_name, use_tenary_op);
            }
        }

        // Intrinsics
        ResourceIr::Base64(base64) => match base64.as_ref() {
            ResourceIr::String(b64) => {
                context.import_buffer();
                output.text(format!(
                    "Buffer.from('{}', 'base64').toString('binary')",
                    b64.escape_debug()
                ))
            }
            other => {
                output.text("cdk.Fn.base64(");
                emit_resource_ir(context, output, other, None, false, use_tenary_op);
                output.text(")")
            }
        },
        ResourceIr::Cidr(ip_range, count, mask) => {
            output.text("cdk.Fn.cidr(");
            emit_resource_ir(context, output, ip_range, None, false, use_tenary_op);
            output.text(", ");
            emit_resource_ir(context, output, count, None, false, use_tenary_op);
            output.text(", String(");
            emit_resource_ir(context, output, mask, None, false, use_tenary_op);
            output.text("))")
        }
        ResourceIr::GetAZs(region) => {
            output.text("cdk.Fn.getAzs(");
            emit_resource_ir(context, output, region, None, false, use_tenary_op);
            output.text(")")
        }
        ResourceIr::If(cond_name, if_true, if_false) => {
            if use_tenary_op {
                output.text(format!("{} ? ", pretty_name(cond_name)));
                emit_resource_ir(context, output, if_true, None, keep_prop_name, use_tenary_op);
                output.text(" : ");
                emit_resource_ir(context, output, if_false, None, keep_prop_name, use_tenary_op);
            } else {
                output.text(format!("cdk.Fn.conditionIf('{}', ", pascal_case(cond_name)));
                emit_resource_ir(context, output, if_true, None, keep_prop_name, use_tenary_op);
                output.text(", ");
                emit_resource_ir(context, output, if_false, None, keep_prop_name, use_tenary_op);
                output.text(")");
            }
        }
        ResourceIr::ImportValue(name) => {
            output.text(format!("cdk.Fn.importValue('{}')", name.escape_debug()))
        }
        ResourceIr::Join(sep, list) => {
            let items = output.indent_with_options(IndentOptions {
                indent: INDENT,
                leading: Some("[".into()),
                trailing: Some(format!("].join('{sep}')", sep = sep.escape_debug()).into()),
                trailing_newline: false,
            });
            for item in list {
                emit_resource_ir(context, &items, item, Some(",\n"), false, use_tenary_op);
            }
        }
        ResourceIr::Map(name, tlk, slk) => {
            output.text(format!("{}[", pretty_name(name)));
            emit_resource_ir(context, output, tlk, None, false, use_tenary_op);
            output.text("][");
            emit_resource_ir(context, output, slk, None, false, use_tenary_op);
            output.text("]")
        }
        ResourceIr::Select(idx, list) => match list.as_ref() {
            ResourceIr::Array(_, array) => {
                if *idx <= array.len() {
                    emit_resource_ir(context, output, &array[*idx], None, false, use_tenary_op);
                } else {
                    output.text("undefined")
                }
            }
            other => {
                output.text("cdk.Fn.select(");
                output.text(idx.to_string());
                output.text(", ");
                emit_resource_ir(context, output, other, None, false, use_tenary_op);
                output.text(")")
            }
        },
        ResourceIr::Split(sep, str) => match str.as_ref() {
            ResourceIr::String(str) => {
                output.text(format!("'{str}'", str = str.escape_debug()));
                output.text(format!(".split('{sep}')", sep = sep.escape_debug()))
            }
            other => {
                output.text(format!("cdk.Fn.split('{sep}', ", sep = sep.escape_debug()));
                emit_resource_ir(context, output, other, None, false, use_tenary_op);
                output.text(")")
            }
        },
        ResourceIr::Sub(parts) => {
            output.text("cdk.Fn.sub('");
            for part in parts {
                match part {
                    ResourceIr::String(lit) => output.text(lit.clone()),
                    ResourceIr::Ref(reference) => {
                        output.text("${");
                        match &reference.origin {
                            Origin::PseudoParameter(x) => match x {
                                PseudoParameter::Partition => output.text("AWS::Partition"),
                                PseudoParameter::Region => output.text("AWS::Region"),
                                PseudoParameter::StackId => output.text("AWS::StackId"),
                                PseudoParameter::StackName => output.text("AWS::StackName"),
                                PseudoParameter::URLSuffix => output.text("AWS::URLSuffix"),
                                PseudoParameter::AccountId => output.text("AWS::AccountId"),
                                PseudoParameter::NotificationArns => output.text("AWS::NotificationArns"),
                            },
                            Origin::Parameter => output.text(format!("{}", pascal_case(&reference.name))),
                            _other => output.text(reference.to_typescript()),
                        };
                        output.text("}");
                    },
                    other => {
                        output.text("${");
                        emit_resource_ir(context, output, other, None, false, use_tenary_op);
                        output.text("}");
                    }
                }
            }
            output.text("')")
        }

        // References
        ResourceIr::Ref(reference) => output.text(reference.to_typescript()),
    }

    if let Some(trailer) = trailer {
        output.text(trailer.to_owned())
    }
}

fn emit_parameters(output: &CodeBuffer, parameters: &[ConstructorParameter]) {
    if parameters.is_empty() {
        return;
    }

    output.newline();
    output.line("// Parameters");

    for parameter in parameters {
        let var_name = pretty_name(&parameter.name);
        let output = output.indent_with_options(IndentOptions {
            indent: INDENT,
            leading: Some(
                format!(
                    "const {var_name}Parameter = new cdk.CfnParameter(this, \"{pascal_var_name}\", {{",
                    pascal_var_name = pascal_case(&var_name)
                )
                .into(),
            ),
            trailing: Some("});".into()),
            trailing_newline: true,
        });
        if parameter.default_value.is_some() {
            let default_value = parameter.default_value.as_ref().unwrap();
            output.line(format!("default: \"{}\",", default_value));
            if default_value == "true" || default_value == "false" {
                output.line("allowedValues:[\"true\", \"false\"],");
            }
        }
        if parameter.description.is_some() {
            output.line(format!("description: \"{}\",", parameter.description.as_ref().unwrap()));
        }
        output.line(format!("type: \"{}\",", parameter.constructor_type));
    }
}

fn emit_mappings(output: &CodeBuffer, mappings: &[MappingInstruction]) {
    if mappings.is_empty() {
        return;
    }

    output.newline();
    output.line("// Mappings");

    for mapping in mappings {
        let item_type = match mapping.output_type() {
            OutputType::Consistent(inner_type) => match inner_type {
                MappingInnerValue::Number(_) | MappingInnerValue::Float(_) => "number",
                MappingInnerValue::Bool(_) => "boolean",
                MappingInnerValue::String(_) => "string",
                MappingInnerValue::List(_) => "readonly string[]",
            },
            OutputType::Complex => "any",
        };

        let var_name = pretty_name(&mapping.name);
        let output = output.indent_with_options(IndentOptions {
            indent: INDENT,
            leading: Some(
                format!(
                    "const {var_name}: Record<string, Record<string, {item_type}>> = {{"
                )
                .into(),
            ),
            trailing: Some(
                format!(
                    "}};\nnew cdk.CfnMapping(this, \"{pascal_var_name}\", {{ mapping: {var_name} }});",
                    pascal_var_name = pascal_case(&var_name)
                ).into(),
            ),
            trailing_newline: true,
        });

        emit_mapping_instruction(output, mapping);
    }
}

fn emit_conditions(output: &CodeBuffer, conditions: &[ConditionInstruction]) {
    if !conditions.is_empty() {
        output.newline();
        output.line("// Conditions");

        for cond in conditions {
            let cond_name = &cond.name;
            let output = output.indent_with_options(IndentOptions {
                indent: INDENT,
                leading: Some(
                    format!(
                        "const {var_name}Condition = new cdk.CfnCondition(this, \"{cond_name}\", {{ expression: ",
                        var_name = pretty_name(cond_name),
                    )
                    .into(),
                ),
                trailing: Some("});".into()),
                trailing_newline: true,
            });

            let synthed = synthesize_cfn_condition_recursive(&cond.value);
            output.line(synthed);
        }
    }    
}

fn synthesize_condition_recursive(val: &ConditionIr) -> String {
    match val {
        ConditionIr::And(x) => {
            let a: Vec<String> = x.iter().map(synthesize_condition_recursive).collect();

            let inner = a.join(" && ");
            format!("({inner})")
        }
        ConditionIr::Equals(a, b) => {
            format!(
                "{} === {}",
                synthesize_condition_recursive(a.as_ref()),
                synthesize_condition_recursive(b.as_ref())
            )
        }
        ConditionIr::Not(x) => {
            if x.is_simple() {
                format!("!{}", synthesize_condition_recursive(x.as_ref()))
            } else {
                format!("!({})", synthesize_condition_recursive(x.as_ref()))
            }
        }
        ConditionIr::Or(x) => {
            let a: Vec<String> = x.iter().map(synthesize_condition_recursive).collect();

            let inner = a.join(" || ");
            format!("({inner})")
        }
        ConditionIr::Str(x) => {
            format!("'{x}'")
        }
        ConditionIr::Condition(x) => pretty_name(x),
        ConditionIr::Ref(x) => x.to_typescript().into(),
        ConditionIr::Map(named_resource, l1, l2) => {
            format!(
                "{}[{}][{}]",
                pretty_name(named_resource),
                synthesize_condition_recursive(l1.as_ref()),
                synthesize_condition_recursive(l2.as_ref())
            )
        }
        ConditionIr::Split(sep, l1) => {
            let str = synthesize_condition_recursive(l1.as_ref());
            format!(
                "{str}.split('{sep}')",
                str = str.escape_debug(),
                sep = sep.escape_debug()
            )
        }
        ConditionIr::Select(index, l1) => {
            let str = synthesize_condition_recursive(l1.as_ref());
            format!("cdk.Fn.select({index}, {str})")
        }
    }
}

fn synthesize_cfn_condition_recursive(val: &ConditionIr) -> String {
    match val {
        ConditionIr::And(x) => {
            let a: Vec<String> = x.iter().map(synthesize_cfn_condition_recursive).collect();

            let inner = a.join(",\n");
            format!("cdk.Fn.conditionAnd({inner})")
        }
        ConditionIr::Equals(a, b) => {
            format!(
                "cdk.Fn.conditionEquals({}, {})",
                synthesize_cfn_condition_recursive(a.as_ref()),
                synthesize_cfn_condition_recursive(b.as_ref())
            )
        }
        ConditionIr::Not(x) => {
            format!("cdk.Fn.conditionNot({})", synthesize_cfn_condition_recursive(x.as_ref()))
        }
        ConditionIr::Or(x) => {
            let a: Vec<String> = x.iter().map(synthesize_cfn_condition_recursive).collect();

            let inner = a.join(",\n");
            format!("cdk.Fn.conditionOr({inner})")
        }
        ConditionIr::Str(x) => {
            format!("'{x}'")
        }
        ConditionIr::Condition(x) => format!("{}", pretty_name(x)),
        ConditionIr::Ref(x) => match &x.origin {
            Origin::Condition => format!("{}Condition", camel_case(&x.name)).into(),
            Origin::Parameter => format!("{}Parameter", camel_case(&x.name)).into(),
            Origin::PseudoParameter(param) => match param {
                PseudoParameter::Partition => "cdk.Aws.PARTITION".into(),
                PseudoParameter::Region => "cdk.Aws.REGION".into(),
                PseudoParameter::StackId => "cdk.Aws.STACK_ID".into(),
                PseudoParameter::StackName => "cdk.Aws.STACK_NAME".into(),
                PseudoParameter::URLSuffix => "cdk.Aws.URL_SUFFIX".into(),
                PseudoParameter::AccountId => "cdk.Aws.ACCOUNT_ID".into(),
                PseudoParameter::NotificationArns => "cdk.Aws.NOTIFICATION_ARNS".into(),
            },
            _other => x.to_typescript().into()
        }
        ConditionIr::Map(named_resource, l1, l2) => {
            format!(
                "cdk.Fn.findInMap('{}', {}, {})",
                pascal_case(named_resource),
                synthesize_cfn_condition_recursive(l1.as_ref()),
                synthesize_cfn_condition_recursive(l2.as_ref())
            )
        }
        ConditionIr::Split(sep, l1) => {
            let str = synthesize_cfn_condition_recursive(l1.as_ref());
            format!(
                "cdk.Fn.split('{sep}', {str})",
                str = str.escape_debug(),
                sep = sep.escape_debug()
            )
        }
        ConditionIr::Select(index, l1) => {
            let str = synthesize_cfn_condition_recursive(l1.as_ref());
            format!("cdk.Fn.select({index}, {str})")
        }
    }
}

fn emit_mapping_instruction(output: Rc<CodeBuffer>, mapping_instruction: &MappingInstruction) {
    for (name, inner_mapping) in &mapping_instruction.map {
        let output = output.indent_with_options(IndentOptions {
            indent: INDENT,
            leading: Some(format!("'{key}': {{", key = name.escape_debug()).into()),
            trailing: Some("},".into()),
            trailing_newline: true,
        });
        emit_inner_mapping(output, inner_mapping);
    }
}

fn emit_inner_mapping(output: Rc<CodeBuffer>, inner_mapping: &IndexMap<String, MappingInnerValue>) {
    for (name, value) in inner_mapping {
        output.line(format!("'{key}': {value},", key = name.escape_debug()));
    }
}

fn append_references(output: &CodeBuffer, reference: &ResourceInstruction) {
    for dep in &reference.references {
        output.line(format!("if ({dep} == null) {{ throw new Error(`A combination of conditions caused '{dep}' to be undefined. Fixit.`); }}", dep=pretty_name(dep)));
    }
}

struct SuffixFix {
    suffix: &'static str,
    fix: &'static str,
}

/// If you have stumbled across this lunacy, I still don't fully understand it myself.
///
/// CDK folks decided to prettify a few names, e.g. ProviderARNs -> providerArns.
/// This list is hand-maintained, but always refer to the original source:
///
static SUFFIX_FIXES: &[SuffixFix] = &[
    SuffixFix {
        suffix: "ARNs",
        fix: "Arns",
    },
    SuffixFix {
        suffix: "MBs",
        fix: "MBs",
    },
    SuffixFix {
        suffix: "AZs",
        fix: "AZs",
    },
];

fn pretty_name(name: &str) -> String {
    // hardcoded consts that always need love.
    if name == "VPCs" {
        return "vpcs".to_string();
    }
    if name == "GetObject" {
        return "objectAccess".to_string();
    }
    if name == "Equals" {
        return "equalTo".to_string();
    }

    let mut end_str = name.to_string();
    for hay in SUFFIX_FIXES {
        if end_str.ends_with(hay.suffix) {
            end_str = end_str[0..end_str.len() - hay.suffix.len()].to_string();
            end_str.push_str(hay.fix);
            break;
        }
    }

    camel_case(&end_str)
}

trait TypescriptCodeBuffer {
    fn tsdoc(&self) -> Rc<CodeBuffer>;
}

impl TypescriptCodeBuffer for CodeBuffer {
    #[inline]
    fn tsdoc(&self) -> Rc<CodeBuffer> {
        self.indent_with_options(IndentOptions {
            indent: " * ".into(),
            leading: Some("/**".into()),
            trailing: Some(" */".into()),
            trailing_newline: true,
        })
    }
}

#[cfg(test)]
mod tests;
