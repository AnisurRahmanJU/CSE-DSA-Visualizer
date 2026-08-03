
const KEYWORDS = new Set([
  'int','float','double','char','void','if','else','while','do','for',
  'return','break','continue','struct'
]);

function lexC(src){
  const tokens = [];
  let i = 0, line = 1;
  const n = src.length;

  function peek(o){ return src[i+(o||0)]; }

  while(i<n){
    const c = src[i];

    // newline
    if(c === '\n'){ line++; i++; continue; }
    // whitespace
    if(c === ' ' || c === '\t' || c === '\r'){ i++; continue; }

    // preprocessor line — skip to end of line, still count the newline
    if(c === '#'){
      while(i<n && src[i] !== '\n') i++;
      continue;
    }

    // line comment
    if(c === '/' && peek(1) === '/'){
      while(i<n && src[i] !== '\n') i++;
      continue;
    }
    // block comment
    if(c === '/' && peek(1) === '*'){
      i += 2;
      while(i<n && !(src[i]==='*' && peek(1)==='/')){
        if(src[i]==='\n') line++;
        i++;
      }
      i += 2;
      continue;
    }

    // string literal
    if(c === '"'){
      let start = i; let startLine=line; i++;
      let out = '';
      while(i<n && src[i] !== '"'){
        if(src[i] === '\\'){
          const esc = src[i+1];
          const map = {n:'\n', t:'\t', '\\':'\\', '"':'"', '0':'\0', r:'\r', "'":"'"};
          out += (esc in map) ? map[esc] : esc;
          i += 2;
        } else {
          out += src[i]; i++;
        }
      }
      i++; // closing quote
      tokens.push({type:'STRING', value: out, line:startLine});
      continue;
    }

    // char literal
    if(c === "'"){
      let startLine=line; i++;
      let val;
      if(src[i] === '\\'){
        const esc = src[i+1];
        const map = {n:10, t:9, '0':0, '\\':92, "'":39, '"':34, r:13};
        val = (esc in map) ? map[esc] : esc.charCodeAt(0);
        i += 2;
      } else {
        val = src.charCodeAt(i);
        i += 1;
      }
      i++; // closing quote
      tokens.push({type:'CHAR', value: val, line:startLine});
      continue;
    }

    // number
    if(/[0-9]/.test(c) || (c==='.' && /[0-9]/.test(peek(1)))){
      let start=i; let startLine=line; let isFloat=false;
      while(i<n && /[0-9]/.test(src[i])) i++;
      if(src[i]==='.'){ isFloat=true; i++; while(i<n && /[0-9]/.test(src[i])) i++; }
      // skip trailing suffixes like f, L, u
      while(i<n && /[fFlLuU]/.test(src[i])) i++;
      const raw = src.slice(start,i).replace(/[fFlLuU]+$/,'');
      tokens.push({type:'NUMBER', value: parseFloat(raw), isFloat, line:startLine});
      continue;
    }

    // identifier / keyword
    if(/[A-Za-z_]/.test(c)){
      let start=i; let startLine=line;
      while(i<n && /[A-Za-z0-9_]/.test(src[i])) i++;
      const word = src.slice(start,i);
      tokens.push({type: KEYWORDS.has(word) ? 'KEYWORD' : 'IDENT', value: word, line:startLine});
      continue;
    }

    // multi-char punctuators
    const three = src.slice(i,i+3);
    if(['<<=','>>='].includes(three)){ tokens.push({type:'PUNCT', value:three, line}); i+=3; continue; }
    const two = src.slice(i,i+2);
    if(['==','!=','<=','>=','&&','||','++','--','+=','-=','*=','/=','%=','->','<<','>>'].includes(two)){
      tokens.push({type:'PUNCT', value:two, line}); i+=2; continue;
    }
    if('+-*/%=<>!&|^~?:;,(){}[].'.includes(c)){
      tokens.push({type:'PUNCT', value:c, line}); i++; continue;
    }

    // unknown char — skip it defensively
    i++;
  }
  tokens.push({type:'EOF', value:null, line});
  return tokens;
}



/* ============================================================
   Parser: tokens -> AST
   Every statement/expression node carries a `line` for the
   line-by-line execution indicator.
   ============================================================ */

const TYPE_KEYWORDS = new Set(['int','float','double','char','void']);

function parseC(src){
  const tokens = lexC(src);
  let pos = 0;

  function cur(){ return tokens[pos]; }
  function la(o){ return tokens[pos+o]; }
  function atEnd(){ return cur().type === 'EOF'; }

  function check(type, value){
    const t = cur();
    if(t.type !== type) return false;
    if(value !== undefined && t.value !== value) return false;
    return true;
  }
  function checkPunct(v){ return check('PUNCT', v); }
  function checkKw(v){ return check('KEYWORD', v); }

  function advance(){ const t = cur(); if(!atEnd()) pos++; return t; }

  function expectPunct(v){
    if(!checkPunct(v)) throw new CParseError(`Expected "${v}" but found "${cur().value}"`, cur().line);
    return advance();
  }
  function expectKw(v){
    if(!checkKw(v)) throw new CParseError(`Expected keyword "${v}"`, cur().line);
    return advance();
  }
  function expectIdent(){
    if(!check('IDENT')) throw new CParseError(`Expected an identifier but found "${cur().value}"`, cur().line);
    return advance().value;
  }

  function isTypeStart(){
    return check('KEYWORD') && TYPE_KEYWORDS.has(cur().value);
  }

  /* ---------------- Program ---------------- */
  function parseProgram(){
    const decls = [];
    while(!atEnd()){
      decls.push(parseTopLevel());
    }
    return { type:'Program', decls };
  }

  function parseTopLevel(){
    if(!isTypeStart()) throw new CParseError(`Expected a type (int/float/char/void) at top level, found "${cur().value}"`, cur().line);
    const typeTok = advance();
    const name = expectIdent();
    if(checkPunct('(')){
      // function definition
      const line = typeTok.line;
      advance(); // (
      const params = [];
      if(!checkPunct(')')){
        do {
          if(!isTypeStart()) throw new CParseError('Expected a parameter type', cur().line);
          const pType = advance().value;
          const pName = expectIdent();
          let isArray = false;
          if(checkPunct('[')){ advance(); if(checkPunct(']')) advance(); isArray = true; }
          params.push({ptype:pType, name:pName, isArray});
        } while(checkPunct(',') && advance());
      }
      expectPunct(')');
      const body = parseBlock();
      return { type:'FunctionDecl', name, returnType:typeTok.value, params, body, line };
    } else {
      // global variable declaration
      const decl = parseVarDeclTail(typeTok.value, typeTok.line, name);
      expectPunct(';');
      return decl;
    }
  }

  /* ---------------- Statements ---------------- */
  function parseBlock(){
    const line = cur().line;
    expectPunct('{');
    const stmts = [];
    while(!checkPunct('}') && !atEnd()){
      stmts.push(parseStatement());
    }
    expectPunct('}');
    return { type:'Block', stmts, line };
  }

  function parseStatement(){
    if(checkPunct('{')) return parseBlock();
    if(isTypeStart()) return parseVarDeclStatement();
    if(checkKw('if')) return parseIf();
    if(checkKw('while')) return parseWhile();
    if(checkKw('do')) return parseDoWhile();
    if(checkKw('for')) return parseFor();
    if(checkKw('return')) return parseReturn();
    if(checkKw('break')){ const line=advance().line; expectPunct(';'); return {type:'Break', line}; }
    if(checkKw('continue')){ const line=advance().line; expectPunct(';'); return {type:'Continue', line}; }
    if(checkPunct(';')){ const line=advance().line; return {type:'Empty', line}; }
    // expression statement
    const line = cur().line;
    const expr = parseExpr();
    expectPunct(';');
    return { type:'ExprStmt', expr, line };
  }

  function parseVarDeclStatement(){
    const typeTok = advance();
    const name = expectIdent();
    const decl = parseVarDeclTail(typeTok.value, typeTok.line, name);
    expectPunct(';');
    return decl;
  }

  // Parses one or more comma-separated declarators of the given base type,
  // where the FIRST identifier has already been consumed (passed as firstName).
  function parseVarDeclTail(baseType, line, firstName){
    const declarators = [];
    declarators.push(parseDeclaratorRest(firstName));
    while(checkPunct(',')){
      advance();
      const name = expectIdent();
      declarators.push(parseDeclaratorRest(name));
    }
    return { type:'VarDecl', baseType, declarators, line };
  }

  function parseDeclaratorRest(name){
    let arraySize = null;
    let isArray = false;
    if(checkPunct('[')){
      isArray = true;
      advance();
      if(!checkPunct(']')) arraySize = parseExpr();
      expectPunct(']');
    }
    let init = null;
    if(checkPunct('=')){
      advance();
      if(checkPunct('{')){
        init = parseArrayLiteral();
      } else {
        init = parseAssignment();
      }
    }
    return { name, isArray, arraySize, init };
  }

  function parseArrayLiteral(){
    const line = cur().line;
    expectPunct('{');
    const elements = [];
    if(!checkPunct('}')){
      do { elements.push(parseAssignment()); } while(checkPunct(',') && advance());
    }
    expectPunct('}');
    return { type:'ArrayLiteral', elements, line };
  }

  function parseIf(){
    const line = advance().line; // if
    expectPunct('(');
    const cond = parseExpr();
    expectPunct(')');
    const then = parseStatement();
    let alt = null;
    if(checkKw('else')){ advance(); alt = parseStatement(); }
    return { type:'If', cond, then, alt, line };
  }
  function parseWhile(){
    const line = advance().line;
    expectPunct('(');
    const cond = parseExpr();
    expectPunct(')');
    const body = parseStatement();
    return { type:'While', cond, body, line };
  }
  function parseDoWhile(){
    const line = advance().line;
    const body = parseStatement();
    expectKw('while');
    expectPunct('(');
    const cond = parseExpr();
    expectPunct(')');
    expectPunct(';');
    return { type:'DoWhile', cond, body, line };
  }
  function parseFor(){
    const line = advance().line;
    expectPunct('(');
    let init = null;
    if(!checkPunct(';')){
      if(isTypeStart()){
        const typeTok = advance();
        const name = expectIdent();
        init = parseVarDeclTail(typeTok.value, typeTok.line, name);
      } else {
        init = { type:'ExprStmt', expr: parseExpr(), line: cur().line };
      }
    }
    expectPunct(';');
    let cond = null;
    if(!checkPunct(';')) cond = parseExpr();
    expectPunct(';');
    let update = null;
    if(!checkPunct(')')) update = parseExpr();
    expectPunct(')');
    const body = parseStatement();
    return { type:'For', init, cond, update, body, line };
  }
  function parseReturn(){
    const line = advance().line;
    let expr = null;
    if(!checkPunct(';')) expr = parseExpr();
    expectPunct(';');
    return { type:'Return', expr, line };
  }

  /* ---------------- Expressions (precedence climbing) ---------------- */
  function parseExpr(){ return parseComma(); }
  function parseComma(){
    let e = parseAssignment();
    while(checkPunct(',')){
      const line = advance().line;
      const rhs = parseAssignment();
      e = { type:'Comma', left:e, right:rhs, line };
    }
    return e;
  }
  const ASSIGN_OPS = new Set(['=','+=','-=','*=','/=','%=']);
  function parseAssignment(){
    const left = parseTernary();
    if(check('PUNCT') && ASSIGN_OPS.has(cur().value)){
      const op = advance().value;
      const line = left.line;
      const right = parseAssignment();
      return { type:'Assign', op, target:left, value:right, line };
    }
    return left;
  }
  function parseTernary(){
    const cond = parseLogicalOr();
    if(checkPunct('?')){
      const line = advance().line;
      const cons = parseAssignment();
      expectPunct(':');
      const alt = parseAssignment();
      return { type:'Ternary', cond, cons, alt, line };
    }
    return cond;
  }
  function parseLogicalOr(){
    let e = parseLogicalAnd();
    while(checkPunct('||')){ const line=advance().line; const r=parseLogicalAnd(); e={type:'Logical',op:'||',left:e,right:r,line}; }
    return e;
  }
  function parseLogicalAnd(){
    let e = parseBitOr();
    while(checkPunct('&&')){ const line=advance().line; const r=parseBitOr(); e={type:'Logical',op:'&&',left:e,right:r,line}; }
    return e;
  }
  function parseBitOr(){
    let e = parseBitXor();
    while(checkPunct('|')){ const line=advance().line; const r=parseBitXor(); e={type:'Binary',op:'|',left:e,right:r,line}; }
    return e;
  }
  function parseBitXor(){
    let e = parseBitAnd();
    while(checkPunct('^')){ const line=advance().line; const r=parseBitAnd(); e={type:'Binary',op:'^',left:e,right:r,line}; }
    return e;
  }
  function parseBitAnd(){
    let e = parseEquality();
    while(checkPunct('&')){ const line=advance().line; const r=parseEquality(); e={type:'Binary',op:'&',left:e,right:r,line}; }
    return e;
  }
  function parseEquality(){
    let e = parseRelational();
    while(checkPunct('==') || checkPunct('!=')){
      const op = advance().value; const line=e.line; const r = parseRelational();
      e = { type:'Binary', op, left:e, right:r, line };
    }
    return e;
  }
  function parseRelational(){
    let e = parseAdditive();
    while(checkPunct('<')||checkPunct('>')||checkPunct('<=')||checkPunct('>=')){
      const op = advance().value; const line=e.line; const r = parseAdditive();
      e = { type:'Binary', op, left:e, right:r, line };
    }
    return e;
  }
  function parseAdditive(){
    let e = parseMultiplicative();
    while(checkPunct('+')||checkPunct('-')){
      const op = advance().value; const line=e.line; const r = parseMultiplicative();
      e = { type:'Binary', op, left:e, right:r, line };
    }
    return e;
  }
  function parseMultiplicative(){
    let e = parseUnary();
    while(checkPunct('*')||checkPunct('/')||checkPunct('%')){
      const op = advance().value; const line=e.line; const r = parseUnary();
      e = { type:'Binary', op, left:e, right:r, line };
    }
    return e;
  }
  function parseUnary(){
    if(checkPunct('!')||checkPunct('-')||checkPunct('+')||checkPunct('~')){
      const op = advance().value; const line = cur().line; const e = parseUnary();
      return { type:'Unary', op, expr:e, line };
    }
    if(checkPunct('++')||checkPunct('--')){
      const op = advance().value; const line = cur().line; const e = parseUnary();
      return { type:'PreIncDec', op, target:e, line };
    }
    if(checkPunct('&')){
      // address-of: we support this ONLY as an argument to scanf, tolerated
      // syntactically and treated as a reference to the named variable.
      const line = advance().line;
      const e = parseUnary();
      return { type:'AddressOf', target:e, line };
    }
    return parsePostfix();
  }
  function parsePostfix(){
    let e = parsePrimary();
    for(;;){
      if(checkPunct('[')){
        const line = advance().line;
        const idx = parseExpr();
        expectPunct(']');
        e = { type:'Index', array:e, index:idx, line };
      } else if(checkPunct('(')){
        const line = advance().line;
        const args = [];
        if(!checkPunct(')')){
          do { args.push(parseAssignment()); } while(checkPunct(',') && advance());
        }
        expectPunct(')');
        e = { type:'Call', callee:e, args, line };
      } else if(checkPunct('++') || checkPunct('--')){
        const op = advance().value;
        e = { type:'PostIncDec', op, target:e, line:e.line };
      } else break;
    }
    return e;
  }
  function parsePrimary(){
    const t = cur();
    if(t.type==='NUMBER'){ advance(); return { type:'Number', value:t.value, isFloat:t.isFloat, line:t.line }; }
    if(t.type==='STRING'){ advance(); return { type:'String', value:t.value, line:t.line }; }
    if(t.type==='CHAR'){ advance(); return { type:'CharLit', value:t.value, line:t.line }; }
    if(t.type==='IDENT'){ advance(); return { type:'Ident', name:t.value, line:t.line }; }
    if(checkPunct('(')){
      advance();
      const e = parseExpr();
      expectPunct(')');
      return e;
    }
    throw new CParseError(`Unexpected token "${t.value===null?'end of input':t.value}"`, t.line);
  }

  const program = parseProgram();
  return program;
}

class CParseError extends Error {
  constructor(msg, line){ super(msg); this.line = line; this.name='CParseError'; }
}



/* ============================================================
   Interpreter: walks the AST, executes the supported C subset,
   and records one "step" snapshot per executed statement/decision
   so the UI can play them back like a debugger.
   ============================================================ */

class CRuntimeError extends Error {
  constructor(msg, line){ super(msg); this.line = line; this.name='CRuntimeError'; }
}
class BreakSignal {}
class ContinueSignal {}
class ReturnSignal { constructor(value){ this.value = value; } }

const MAX_STEPS = 6000;
const MAX_CALL_DEPTH = 200;

function isArrayVal(v){ return Array.isArray(v); }

function runC(source, opts){
  opts = opts || {};
  const stdinQueue = (opts.stdin || []).slice();
  const steps = [];
  let output = '';
  let stepCount = 0;
  let aborted = null;

  let ast;
  try {
    ast = parseC(source);
  } catch(e){
    if(e instanceof CParseError){
      return { ok:false, error: e.message, line: e.line, steps: [] };
    }
    return { ok:false, error: 'Parse error: '+e.message, line: null, steps: [] };
  }

  const functions = {};
  const globalScope = { vars: new Map(), parent: null };

  for(const decl of ast.decls){
    if(decl.type === 'FunctionDecl') functions[decl.name] = decl;
    else if(decl.type === 'VarDecl') execVarDecl(decl, globalScope);
  }

  if(!functions.main){
    return { ok:false, error: 'No main() function found. Every program needs an int main() function.', line:null, steps: [] };
  }

  // ---- call stack of frames for visualization ----
  // each frame: { fn, scope, args (for display) }
  const callStack = [];
  // ---- historical function call tree (like Programiz's function visualizer) ----
  // grows for the whole run; nodes never get removed once created, only their
  // status/label change when they return.
  const callTreeNodes = [];
  let callTreeIdSeq = 0;

  function newScope(parent){ return { vars: new Map(), parent }; }

  function findScopeWithVar(scope, name){
    let s = scope;
    while(s){
      if(s.vars.has(name)) return s;
      s = s.parent;
    }
    return null;
  }

  function getVar(scope, name, line){
    const s = findScopeWithVar(scope, name);
    if(!s) throw new CRuntimeError(`"${name}" is not declared.`, line);
    return s.vars.get(name);
  }
  function setVar(scope, name, value, line){
    const s = findScopeWithVar(scope, name);
    if(!s) throw new CRuntimeError(`"${name}" is not declared.`, line);
    s.vars.set(name, value);
  }
  function declareVar(scope, name, value){
    scope.vars.set(name, value);
  }

  function snapshotFrames(){
    return callStack.map(f => ({
      fn: f.fn,
      locals: scopeChainToFlatObject(f.scope, f.fnScope)
    }));
  }
  // Flattens all nested block scopes up to (and including) the function's
  // own scope into one object for display purposes.
  function scopeChainToFlatObject(scope, stopAt){
    const out = {};
    let s = scope;
    const chain = [];
    while(s){ chain.push(s); if(s===stopAt) break; s = s.parent; }
    for(let i=chain.length-1;i>=0;i--){
      for(const [k,v] of chain[i].vars.entries()){
        out[k] = isArrayVal(v) ? '['+v.join(',')+']' : v;
      }
    }
    return out;
  }

  function findArraysInScope(scope, stopAt){
    const arrays = {};
    let s = scope;
    const chain = [];
    while(s){ chain.push(s); if(s===stopAt) break; s = s.parent; }
    for(let i=chain.length-1;i>=0;i--){
      for(const [k,v] of chain[i].vars.entries()){
        if(isArrayVal(v)) arrays[k] = v.slice();
      }
    }
    return arrays;
  }

  function emit(line, desc){
    stepCount++;
    if(stepCount > MAX_STEPS){
      if(!aborted){
        aborted = true;
        steps.push({
          line, desc: 'Stopped: this run produced more than '+MAX_STEPS+' steps (possible infinite loop). Showing the trace so far.',
          output, frames: snapshotFrames(),
          callTree: callTreeNodes.map(n=>Object.assign({},n)),
          arrays: callStack.length ? findArraysInScope(callStack[callStack.length-1].scope, callStack[callStack.length-1].fnScope) : Object.fromEntries([...globalScope.vars].filter(([,v])=>isArrayVal(v)).map(([k,v])=>[k,v.slice()]))
        });
      }
      throw new StepLimitReached();
    }
    const topFrameArrays = callStack.length
      ? findArraysInScope(callStack[callStack.length-1].scope, callStack[callStack.length-1].fnScope)
      : {};
    const globalArrays = {};
    for(const [k,v] of globalScope.vars.entries()) if(isArrayVal(v)) globalArrays[k] = v.slice();

    steps.push({
      line, desc,
      output,
      frames: snapshotFrames(),
      callTree: callTreeNodes.map(n=>Object.assign({},n)),
      arrays: Object.assign({}, globalArrays, topFrameArrays)
    });
  }
  class StepLimitReached extends Error {}

  /* ---------------- Expression evaluation ---------------- */
  function toNum(v){ return typeof v === 'boolean' ? (v?1:0) : v; }

  function evalExpr(node, scope){
    switch(node.type){
      case 'Number': return node.value;
      case 'CharLit': return node.value;
      case 'String': return node.value;
      case 'Ident': return getVar(scope, node.name, node.line);
      case 'AddressOf': return { __ref: node.target.name };
      case 'ArrayLiteral': return node.elements.map(e=>evalExpr(e, scope));
      case 'Comma': evalExpr(node.left, scope); return evalExpr(node.right, scope);
      case 'Ternary': return evalExpr(node.cond, scope) ? evalExpr(node.cons, scope) : evalExpr(node.alt, scope);
      case 'Logical': {
        const l = evalExpr(node.left, scope);
        if(node.op==='&&') return truthy(l) ? (truthy(evalExpr(node.right,scope))?1:0) : 0;
        return truthy(l) ? 1 : (truthy(evalExpr(node.right,scope))?1:0);
      }
      case 'Unary': {
        const v = toNum(evalExpr(node.expr, scope));
        if(node.op==='-') return -v;
        if(node.op==='+') return +v;
        if(node.op==='!') return truthy(v) ? 0 : 1;
        if(node.op==='~') return ~v;
        break;
      }
      case 'Binary': {
        const l = toNum(evalExpr(node.left, scope));
        const r = toNum(evalExpr(node.right, scope));
        switch(node.op){
          case '+': return l+r;
          case '-': return l-r;
          case '*': return l*r;
          case '/':
            if(r===0) throw new CRuntimeError('Division by zero.', node.line);
            return (Number.isInteger(l) && Number.isInteger(r)) ? Math.trunc(l/r) : l/r;
          case '%':
            if(r===0) throw new CRuntimeError('Modulo by zero.', node.line);
            return l % r;
          case '<': return (l<r)?1:0;
          case '>': return (l>r)?1:0;
          case '<=': return (l<=r)?1:0;
          case '>=': return (l>=r)?1:0;
          case '==': return (l===r)?1:0;
          case '!=': return (l!==r)?1:0;
          case '&': return l&r;
          case '|': return l|r;
          case '^': return l^r;
        }
        break;
      }
      case 'PreIncDec': {
        const cur = toNum(readLValue(node.target, scope));
        const next = node.op==='++' ? cur+1 : cur-1;
        writeLValue(node.target, next, scope);
        return next;
      }
      case 'PostIncDec': {
        const cur = toNum(readLValue(node.target, scope));
        const next = node.op==='++' ? cur+1 : cur-1;
        writeLValue(node.target, next, scope);
        return cur;
      }
      case 'Assign': {
        let rhs = evalExpr(node.value, scope);
        if(node.op !== '='){
          const cur = toNum(readLValue(node.target, scope));
          const opMap = {'+=':'+','-=':'-','*=':'*','/=':'/','%=':'%'};
          rhs = applyBinOp(opMap[node.op], cur, toNum(rhs), node.line);
        }
        writeLValue(node.target, rhs, scope);
        return rhs;
      }
      case 'Index': {
        const arr = evalExpr(node.array, scope);
        const idx = toNum(evalExpr(node.index, scope));
        if(!isArrayVal(arr)) throw new CRuntimeError('Indexing a non-array value.', node.line);
        if(idx<0 || idx>=arr.length) throw new CRuntimeError(`Array index ${idx} is out of bounds (size ${arr.length}).`, node.line);
        return arr[idx];
      }
      case 'Call': return evalCall(node, scope);
      default:
        throw new CRuntimeError('Unsupported expression.', node.line);
    }
  }
  function applyBinOp(op,l,r,line){
    switch(op){
      case '+': return l+r; case '-': return l-r; case '*': return l*r;
      case '/': if(r===0) throw new CRuntimeError('Division by zero.', line); return (Number.isInteger(l)&&Number.isInteger(r))?Math.trunc(l/r):l/r;
      case '%': if(r===0) throw new CRuntimeError('Modulo by zero.', line); return l%r;
    }
  }
  function truthy(v){ return toNum(v) !== 0; }

  function readLValue(node, scope){
    if(node.type==='Ident') return getVar(scope, node.name, node.line);
    if(node.type==='Index'){
      const arr = evalExpr(node.array, scope);
      const idx = toNum(evalExpr(node.index, scope));
      if(!isArrayVal(arr)) throw new CRuntimeError('Indexing a non-array value.', node.line);
      if(idx<0 || idx>=arr.length) throw new CRuntimeError(`Array index ${idx} is out of bounds (size ${arr.length}).`, node.line);
      return arr[idx];
    }
    throw new CRuntimeError('Invalid assignment target.', node.line);
  }
  function writeLValue(node, value, scope){
    if(node.type==='Ident'){ setVar(scope, node.name, value, node.line); return; }
    if(node.type==='Index'){
      const arr = evalExpr(node.array, scope);
      const idx = toNum(evalExpr(node.index, scope));
      if(!isArrayVal(arr)) throw new CRuntimeError('Indexing a non-array value.', node.line);
      if(idx<0 || idx>=arr.length) throw new CRuntimeError(`Array index ${idx} is out of bounds (size ${arr.length}).`, node.line);
      arr[idx] = value;
      return;
    }
    throw new CRuntimeError('Invalid assignment target.', node.line);
  }

  /* ---------------- printf / scanf ---------------- */
  function formatPrintf(fmt, args){
    let out=''; let ai=0;
    for(let i=0;i<fmt.length;i++){
      if(fmt[i]==='%'){
        const spec = fmt[i+1];
        if(spec==='%'){ out+='%'; i++; continue; }
        // skip simple width/precision like %5d, %.2f
        let j=i+1;
        while(j<fmt.length && /[0-9.\-+]/.test(fmt[j])) j++;
        const conv = fmt[j];
        const arg = args[ai++];
        if(conv==='d' || conv==='i') out += String(Math.trunc(toNum(arg)));
        else if(conv==='f' || conv==='lf'){ out += Number(toNum(arg)).toFixed(6); }
        else if(conv==='c') out += String.fromCharCode(toNum(arg));
        else if(conv==='s') out += isArrayVal(arg) ? arg.map(x=>String.fromCharCode(x)).join('') : String(arg);
        else out += '';
        i = j;
      } else {
        out += fmt[i];
      }
    }
    return out;
  }

  function evalCall(node, scope){
    const name = node.callee.type==='Ident' ? node.callee.name : null;
    if(name==='printf'){
      const fmt = evalExpr(node.args[0], scope);
      const rest = node.args.slice(1).map(a=>evalExpr(a, scope));
      output += formatPrintf(String(fmt), rest);
      return 0;
    }
    if(name==='scanf'){
      // args[0] is format string; remaining args are &var references
      for(let k=1;k<node.args.length;k++){
        const ref = evalExpr(node.args[k], scope);
        const val = stdinQueue.length ? stdinQueue.shift() : 0;
        if(ref && ref.__ref){ setVar(scope, ref.__ref, Number(val), node.line); }
      }
      return 0;
    }
    if(!name || !functions[name]){
      throw new CRuntimeError(`Unsupported or unknown function call: "${name || '(expression)'}". This visualizer supports user-defined functions plus printf/scanf.`, node.line);
    }
    const fn = functions[name];
    const argVals = node.args.map(a=>evalExpr(a, scope));
    if(callStack.length >= MAX_CALL_DEPTH){
      throw new CRuntimeError('Call stack too deep (possible infinite recursion).', node.line);
    }
    const fnScope = newScope(globalScope);
    fn.params.forEach((p,idx)=>{
      const v = argVals[idx];
      fnScope.vars.set(p.name, isArrayVal(v) ? v : toNum(v));
    });
    const argsLabel = argVals.map(v=>isArrayVal(v) ? 'array' : (Number.isInteger(v) ? v : Number(v).toFixed(2))).join(', ');
    const parentTreeId = callStack.length ? callStack[callStack.length-1].treeNodeId : null;
    const treeId = callTreeIdSeq++;
    callTreeNodes.push({ id: treeId, parentId: parentTreeId, fnName: name, status: 'calling', label: `${name}(${argsLabel})` });
    callStack.push({ fn: name, scope: fnScope, fnScope, treeNodeId: treeId });
    emit(fn.line, `Call ${name}(${argVals.map(v=>isArrayVal(v)?'['+v.join(',')+']':v).join(', ')}).`);
    let result = 0;
    try {
      execBlock(fn.body, fnScope);
    } catch(sig){
      if(sig instanceof ReturnSignal){ result = sig.value; }
      else { callStack.pop(); throw sig; }
    }
    const treeNode = callTreeNodes.find(t=>t.id===treeId);
    if(treeNode){
      treeNode.status = 'done';
      treeNode.label = fn.returnType==='void'
        ? `${name}(${argsLabel})`
        : `${name}(${argsLabel}) = ${Number.isInteger(result) ? result : Number(result).toFixed(2)}`;
    }
    emit(fn.line, `Return from ${name}()${result!==undefined && result!==null ? ' with value '+result : ''}.`);
    callStack.pop();
    return result;
  }

  /* ---------------- Statement execution ---------------- */
  function execVarDecl(decl, scope){
    for(const d of decl.declarators){
      if(d.isArray){
        let arr;
        if(d.init && d.init.type==='ArrayLiteral'){
          arr = d.init.elements.map(e=>toNum(evalExpr(e, scope)));
        } else {
          const size = d.arraySize ? toNum(evalExpr(d.arraySize, scope)) : 0;
          arr = new Array(size).fill(0);
        }
        declareVar(scope, d.name, arr);
      } else {
        const v = d.init ? toNum(evalExpr(d.init, scope)) : 0;
        declareVar(scope, d.name, v);
      }
    }
  }

  function execBlock(block, parentScope){
    const scope = newScope(parentScope);
    for(const stmt of block.stmts){
      execStmt(stmt, scope);
    }
  }

  function describeStmt(stmt, scope){
    switch(stmt.type){
      case 'VarDecl':
        return 'Declare ' + stmt.declarators.map(d=>d.name).join(', ') + '.';
      case 'ExprStmt': return 'Evaluate statement.';
      case 'If': return 'Check condition.';
      case 'While': return 'Check loop condition.';
      case 'DoWhile': return 'Run loop body.';
      case 'For': return 'For-loop step.';
      case 'Return': return 'Return from function.';
      case 'Break': return 'Break out of the loop.';
      case 'Continue': return 'Continue to next iteration.';
      default: return '';
    }
  }

  function execStmt(stmt, scope){
    switch(stmt.type){
      case 'VarDecl':
        execVarDecl(stmt, scope);
        emit(stmt.line, describeStmt(stmt, scope));
        return;
      case 'ExprStmt': {
        evalExpr(stmt.expr, scope);
        emit(stmt.line, exprStmtDesc(stmt.expr, scope));
        return;
      }
      case 'Empty': return;
      case 'Block': execBlock(stmt, scope); return;
      case 'If': {
        const c = truthy(evalExpr(stmt.cond, scope));
        emit(stmt.line, `Condition is ${c ? 'true' : 'false'}.`);
        if(c) execStmt(stmt.then, scope);
        else if(stmt.alt) execStmt(stmt.alt, scope);
        return;
      }
      case 'While': {
        while(true){
          const c = truthy(evalExpr(stmt.cond, scope));
          emit(stmt.line, `Loop condition is ${c?'true':'false'}.`);
          if(!c) break;
          try { execStmt(stmt.body, scope); }
          catch(sig){
            if(sig instanceof BreakSignal) break;
            if(sig instanceof ContinueSignal) continue;
            throw sig;
          }
        }
        return;
      }
      case 'DoWhile': {
        while(true){
          try { execStmt(stmt.body, scope); }
          catch(sig){
            if(sig instanceof BreakSignal) break;
            if(!(sig instanceof ContinueSignal)) throw sig;
          }
          const c = truthy(evalExpr(stmt.cond, scope));
          emit(stmt.line, `Loop condition is ${c?'true':'false'}.`);
          if(!c) break;
        }
        return;
      }
      case 'For': {
        const forScope = newScope(scope);
        if(stmt.init){
          if(stmt.init.type==='VarDecl') execVarDecl(stmt.init, forScope);
          else evalExpr(stmt.init.expr, forScope);
          emit(stmt.line, 'Initialize loop.');
        }
        while(true){
          let c = true;
          if(stmt.cond){ c = truthy(evalExpr(stmt.cond, forScope)); }
          emit(stmt.line, `Loop condition is ${c?'true':'false'}.`);
          if(!c) break;
          try { execStmt(stmt.body, forScope); }
          catch(sig){
            if(sig instanceof BreakSignal) break;
            if(!(sig instanceof ContinueSignal)) throw sig;
          }
          if(stmt.update){ evalExpr(stmt.update, forScope); emit(stmt.line, 'Update loop variable.'); }
        }
        return;
      }
      case 'Return': {
        const v = stmt.expr ? evalExpr(stmt.expr, scope) : 0;
        emit(stmt.line, `Return ${stmt.expr ? 'value '+v : ''}.`);
        throw new ReturnSignal(v);
      }
      case 'Break': emit(stmt.line, 'Break.'); throw new BreakSignal();
      case 'Continue': emit(stmt.line, 'Continue.'); throw new ContinueSignal();
      default:
        throw new CRuntimeError('Unsupported statement.', stmt.line);
    }
  }

  function exprStmtDesc(expr, scope){
    if(expr.type==='Assign'){
      try {
        const v = readLValue(expr.target, scope);
        const tgt = expr.target.type==='Ident' ? expr.target.name : (expr.target.type==='Index' ? (expr.target.array.name||'array')+'['+']' : 'value');
        return `Assign ${tgt} = ${isArrayVal(v)?'['+v.join(',')+']':v}.`;
      } catch(e){ return 'Assignment.'; }
    }
    if(expr.type==='Call'){
      const name = expr.callee.type==='Ident' ? expr.callee.name : 'function';
      return `Call ${name}(...).`;
    }
    if(expr.type==='PostIncDec' || expr.type==='PreIncDec'){
      return `${expr.target.type==='Ident'?expr.target.name:'value'}${expr.op}.`;
    }
    return 'Evaluate expression.';
  }

  /* ---------------- Run main() ---------------- */
  const mainTreeId = callTreeIdSeq++;
  callTreeNodes.push({ id: mainTreeId, parentId: null, fnName: 'main', status: 'calling', label: 'main()' });
  callStack.push({ fn:'main', scope:newScope(globalScope), fnScope:null, treeNodeId: mainTreeId });
  callStack[0].fnScope = callStack[0].scope;
  emit(functions.main.line, 'Program starts: call main().');
  try {
    execBlock(functions.main.body, callStack[0].scope);
  } catch(sig){
    if(sig instanceof ReturnSignal){ /* normal */ }
    else if(sig instanceof StepLimitReached){ /* already recorded */ }
    else if(sig instanceof CRuntimeError){
      return { ok:false, error: sig.message, line: sig.line, steps, output };
    }
    else if(sig instanceof BreakSignal || sig instanceof ContinueSignal){
      return { ok:false, error: 'break/continue used outside of a loop.', line:null, steps, output };
    }
    else {
      return { ok:false, error: String(sig.message||sig), line: sig.line||null, steps, output };
    }
  }
  const mainNode = callTreeNodes.find(t=>t.id===mainTreeId);
  if(mainNode){ mainNode.status='done'; mainNode.label='main() = done'; }
  if(!aborted) emit(functions.main.line, 'main() finished. Program exited.');
  return { ok:true, steps, output };
}



/* =====================================================================
   DSA COMPILER — line-by-line C visualizer
   Part 1: C source blocks + sorting/search trace generators
   ===================================================================== */

function sortedTail(n,i){ const h={}; for(let k=n-i;k<n;k++) h[k]='sorted'; return h; }
function allSortedMap(n){ const h={}; for(let k=0;k<n;k++) h[k]='sorted'; return h; }

/* ---------------- Bubble Sort ---------------- */
const CODE_BUBBLE = [
"void bubbleSort(int arr[], int n) {",
"    int i, j, temp;",
"    for (i = 0; i < n - 1; i++) {",
"        for (j = 0; j < n - i - 1; j++) {",
"            if (arr[j] > arr[j + 1]) {",
"                temp = arr[j];",
"                arr[j] = arr[j + 1];",
"                arr[j + 1] = temp;",
"            }",
"        }",
"    }",
"}"
];
function bubbleSortTrace(arrIn){
  const arr = arrIn.slice(); const n = arr.length; const steps = [];
  const push = (line, desc, hi, vars) => steps.push({ line, desc, arr: arr.slice(), highlights: hi||{}, vars: vars||{} });
  push(1, `bubbleSort called on an array of ${n} elements.`, {}, {n});
  for(let i=0;i<n-1;i++){
    push(3, `Outer pass i=${i}: the largest remaining element will bubble to the end.`, sortedTail(n,i), {i});
    for(let j=0;j<n-i-1;j++){
      push(5, `Compare arr[${j}]=${arr[j]} with arr[${j+1}]=${arr[j+1]}.`,
        Object.assign({[j]:'compare',[j+1]:'compare'}, sortedTail(n,i)), {i,j});
      if(arr[j] > arr[j+1]){
        [arr[j],arr[j+1]] = [arr[j+1],arr[j]];
        push(6, `arr[${j}] > arr[${j+1}] → swapping them.`,
          Object.assign({[j]:'swap',[j+1]:'swap'}, sortedTail(n,i)), {i,j});
      }
    }
    push(10, `Pass i=${i} finished. Value ${arr[n-1-i]} is now fixed in place.`, sortedTail(n,i+1), {i});
  }
  push(12, 'bubbleSort complete — array fully sorted.', allSortedMap(n), {});
  return steps;
}

/* ---------------- Selection Sort ---------------- */
const CODE_SELECTION = [
"void selectionSort(int arr[], int n) {",
"    int i, j, minIdx, temp;",
"    for (i = 0; i < n - 1; i++) {",
"        minIdx = i;",
"        for (j = i + 1; j < n; j++) {",
"            if (arr[j] < arr[minIdx]) {",
"                minIdx = j;",
"            }",
"        }",
"        temp = arr[minIdx];",
"        arr[minIdx] = arr[i];",
"        arr[i] = temp;",
"    }",
"}"
];
function selectionSortTrace(arrIn){
  const arr = arrIn.slice(); const n = arr.length; const steps=[];
  const push=(line,desc,hi,vars)=>steps.push({line,desc,arr:arr.slice(),highlights:hi||{},vars:vars||{}});
  push(1, `selectionSort called on ${n} elements.`, {}, {n});
  for(let i=0;i<n-1;i++){
    let minIdx=i;
    push(4, `Pass i=${i}: assume arr[${i}]=${arr[i]} is the minimum so far.`, Object.assign({[i]:'min'},sortedTail(n,i)), {i,minIdx});
    for(let j=i+1;j<n;j++){
      push(6, `Compare arr[${j}]=${arr[j]} with current minimum arr[${minIdx}]=${arr[minIdx]}.`,
        Object.assign({[j]:'compare',[minIdx]:'min'}, sortedTail(n,i)), {i,j,minIdx});
      if(arr[j]<arr[minIdx]){
        minIdx=j;
        push(7, `New minimum found at index ${j} (value ${arr[j]}).`, Object.assign({[minIdx]:'min'},sortedTail(n,i)), {i,j,minIdx});
      }
    }
    if(minIdx!==i){
      [arr[i],arr[minIdx]]=[arr[minIdx],arr[i]];
    }
    push(11, `Swap arr[${i}] and arr[${minIdx}] to place the minimum at position ${i}.`,
      Object.assign({[i]:'swap',[minIdx]:'swap'}, sortedTail(n,i)), {i,minIdx});
    push(12, `Position ${i} is now finalized with value ${arr[i]}.`, sortedTail(n,i+1), {i});
  }
  push(14, 'selectionSort complete — array fully sorted.', allSortedMap(n), {});
  return steps;
}

/* ---------------- Insertion Sort ---------------- */
const CODE_INSERTION = [
"void insertionSort(int arr[], int n) {",
"    int i, j, key;",
"    for (i = 1; i < n; i++) {",
"        key = arr[i];",
"        j = i - 1;",
"        while (j >= 0 && arr[j] > key) {",
"            arr[j + 1] = arr[j];",
"            j = j - 1;",
"        }",
"        arr[j + 1] = key;",
"    }",
"}"
];
function insertionSortTrace(arrIn){
  const arr = arrIn.slice(); const n=arr.length; const steps=[];
  const push=(line,desc,hi,vars)=>steps.push({line,desc,arr:arr.slice(),highlights:hi||{},vars:vars||{}});
  push(1, `insertionSort called on ${n} elements. arr[0] is trivially sorted.`, n>0?{0:'sorted'}:{}, {n});
  for(let i=1;i<n;i++){
    let key=arr[i]; let j=i-1;
    push(4, `Pick key = arr[${i}] = ${key} to insert into the sorted left portion.`, {[i]:'current'}, {i,j,key});
    while(j>=0 && arr[j]>key){
      push(6, `arr[${j}]=${arr[j]} > key(${key}) → shift it one step right.`, {[j]:'compare',[j+1]:'swap'}, {i,j,key});
      arr[j+1]=arr[j];
      j--;
    }
    arr[j+1]=key;
    const hi={}; for(let k=0;k<=i;k++) hi[k]='sorted';
    push(10, `Insert key=${key} at position ${j+1}. Left portion [0..${i}] is now sorted.`, hi, {i,j,key});
  }
  push(12, 'insertionSort complete — array fully sorted.', allSortedMap(n), {});
  return steps;
}

/* ---------------- Quick Sort ---------------- */
const CODE_QUICK = [
"int partition(int arr[], int lo, int hi) {",
"    int pivot = arr[hi];",
"    int i = lo - 1, j, temp;",
"    for (j = lo; j < hi; j++) {",
"        if (arr[j] < pivot) {",
"            i++;",
"            temp = arr[i]; arr[i] = arr[j]; arr[j] = temp;",
"        }",
"    }",
"    temp = arr[i + 1]; arr[i + 1] = arr[hi]; arr[hi] = temp;",
"    return i + 1;",
"}",
"",
"void quickSort(int arr[], int lo, int hi) {",
"    if (lo < hi) {",
"        int p = partition(arr, lo, hi);",
"        quickSort(arr, lo, p - 1);",
"        quickSort(arr, p + 1, hi);",
"    }",
"}"
];
function quickSortTrace(arrIn){
  const arr = arrIn.slice(); const n=arr.length; const steps=[];
  const push=(line,desc,hi,vars)=>steps.push({line,desc,arr:arr.slice(),highlights:hi||{},vars:vars||{}});
  const finalized = new Set();
  const markFinal = (extra) => { const hi={}; finalized.forEach(k=>hi[k]='sorted'); return Object.assign(hi,extra); };
  function partition(lo,hi){
    const pivot = arr[hi];
    push(2, `Choose arr[${hi}]=${pivot} as the pivot.`, markFinal({[hi]:'pivot'}), {lo,hi,pivot});
    let i=lo-1;
    for(let j=lo;j<hi;j++){
      push(5, `Compare arr[${j}]=${arr[j]} with pivot=${pivot}.`, markFinal({[j]:'compare',[hi]:'pivot'}), {lo,hi,i,j,pivot});
      if(arr[j]<pivot){
        i++;
        [arr[i],arr[j]]=[arr[j],arr[i]];
        push(7, `arr[${j}] < pivot → swap into position ${i}.`, markFinal({[i]:'swap',[j]:'swap',[hi]:'pivot'}), {lo,hi,i,j,pivot});
      }
    }
    [arr[i+1],arr[hi]]=[arr[hi],arr[i+1]];
    push(10, `Place pivot at its correct sorted position ${i+1}.`, markFinal({[i+1]:'swap'}), {lo,hi,pivot});
    finalized.add(i+1);
    return i+1;
  }
  function qs(lo,hi){
    if(lo<hi){
      push(15, `quickSort on sub-array [${lo}..${hi}].`, markFinal({}), {lo,hi});
      const p = partition(lo,hi);
      qs(lo,p-1);
      qs(p+1,hi);
    } else if(lo===hi && lo>=0 && lo<n){ finalized.add(lo); }
  }
  push(14, `quickSort called on ${n} elements.`, {}, {n});
  qs(0,n-1);
  push(19, 'quickSort complete — array fully sorted.', allSortedMap(n), {});
  return steps;
}

/* ---------------- Linear Search ---------------- */
const CODE_LINEAR = [
"int linearSearch(int arr[], int n, int target) {",
"    int i;",
"    for (i = 0; i < n; i++) {",
"        if (arr[i] == target) {",
"            return i;",
"        }",
"    }",
"    return -1;",
"}"
];
function linearSearchTrace(arrIn,target){
  const arr=arrIn.slice(); const n=arr.length; const steps=[];
  const push=(line,desc,hi,vars)=>steps.push({line,desc,arr:arr.slice(),highlights:hi||{},vars:vars||{}});
  push(1, `linearSearch called: looking for target=${target} left to right.`, {}, {target,n});
  for(let i=0;i<n;i++){
    push(4, `Check arr[${i}]=${arr[i]} against target=${target}.`, {[i]:'current'}, {i,target});
    if(arr[i]===target){
      push(5, `Match! arr[${i}] == ${target} → return ${i}.`, {[i]:'found'}, {i,target});
      return steps;
    }
  }
  push(8, `Reached the end without a match → return -1.`, {}, {target});
  return steps;
}

/* ---------------- Binary Search ---------------- */
const CODE_BINARY = [
"int binarySearch(int arr[], int n, int target) {",
"    int lo = 0, hi = n - 1, mid;",
"    while (lo <= hi) {",
"        mid = lo + (hi - lo) / 2;",
"        if (arr[mid] == target) {",
"            return mid;",
"        } else if (arr[mid] < target) {",
"            lo = mid + 1;",
"        } else {",
"            hi = mid - 1;",
"        }",
"    }",
"    return -1;",
"}"
];
function binarySearchTrace(arrIn,target){
  const arr=arrIn.slice().sort((a,b)=>a-b); const n=arr.length; const steps=[];
  const push=(line,desc,hi,vars)=>steps.push({line,desc,arr:arr.slice(),highlights:hi||{},vars:vars||{}});
  let lo=0,hi=n-1;
  push(2, `binarySearch called on the sorted array. lo=${lo}, hi=${hi}.`, {}, {lo,hi,target});
  while(lo<=hi){
    const mid=Math.floor(lo+(hi-lo)/2);
    push(4, `mid = ${lo} + (${hi}-${lo})/2 = ${mid}. arr[mid]=${arr[mid]}.`, {[lo]:'lo',[hi]:'hi',[mid]:'mid'}, {lo,hi,mid,target});
    if(arr[mid]===target){
      push(6, `arr[${mid}] == ${target} → found! return ${mid}.`, {[mid]:'found'}, {lo,hi,mid,target});
      return steps;
    } else if(arr[mid]<target){
      push(8, `arr[${mid}]=${arr[mid]} < ${target} → discard left half, search right.`, {[lo]:'lo',[hi]:'hi',[mid]:'compare'}, {lo,hi,mid,target});
      lo=mid+1;
    } else {
      push(10, `arr[${mid}]=${arr[mid]} > ${target} → discard right half, search left.`, {[lo]:'lo',[hi]:'hi',[mid]:'compare'}, {lo,hi,mid,target});
      hi=mid-1;
    }
  }
  push(13, `lo > hi → target ${target} not found, return -1.`, {}, {lo,hi,target});
  return steps;
}
/* =====================================================================
   Part 2: Merge sort (divide & conquer tree), recursion (call stack /
   recursion tree), and core data structure demos.
   ===================================================================== */

/* ---------------- Merge Sort ---------------- */
const CODE_MERGE = [
"void mergeSort(int arr[], int l, int r) {",
"    if (l < r) {",
"        int m = l + (r - l) / 2;",
"        mergeSort(arr, l, m);",
"        mergeSort(arr, m + 1, r);",
"        merge(arr, l, m, r);",
"    }",
"}",
"",
"void merge(int arr[], int l, int m, int r) {",
"    int i = l, j = m + 1, k = 0, temp[MAX];",
"    while (i <= m && j <= r) {",
"        if (arr[i] <= arr[j]) temp[k++] = arr[i++];",
"        else temp[k++] = arr[j++];",
"    }",
"    while (i <= m) temp[k++] = arr[i++];",
"    while (j <= r) temp[k++] = arr[j++];",
"    for (i = l; i <= r; i++) arr[i] = temp[i - l];",
"}"
];
function mergeSortTrace(arrIn){
  const steps=[]; let nodeId=0; const nodes=[];
  const push=(line,desc,vars)=>steps.push({line,desc,vars:vars||{},nodes:nodes.map(n=>Object.assign({},n))});
  function newNode(parentId,depth,order,arr,status,label){
    const nd={id:nodeId++,parentId,depth,order,arr:arr.slice(),status,label:label||('['+arr.join(',')+']')};
    nodes.push(nd); return nd;
  }
  function setStatus(id,status){ const nd=nodes.find(x=>x.id===id); if(nd) nd.status=status; }
  function setArr(id,arr,label){ const nd=nodes.find(x=>x.id===id); if(nd){ nd.arr=arr.slice(); nd.label=label||('['+arr.join(',')+']'); } }

  push(1, `mergeSort called on the full array of ${arrIn.length} elements.`, {n:arrIn.length});
  const root = newNode(null,0,0,arrIn,'dividing');
  push(2, `l < r is true → this segment has more than one element, so it must be divided.`, {});

  function divide(node, depth, order){
    const arr = node.arr;
    if(arr.length<=1){
      setStatus(node.id,'base');
      push(2, `Base case: segment [${arr.join(',')}] has ${arr.length} element(s) — l < r is false, already sorted.`, {depth});
      return arr.slice();
    }
    const mid = Math.floor(arr.length/2);
    const left = arr.slice(0,mid), right = arr.slice(mid);
    const leftNode = newNode(node.id, depth+1, order*2, left, 'dividing');
    const rightNode = newNode(node.id, depth+1, order*2+1, right, 'dividing');
    push(3, `Compute mid, then split [${arr.join(',')}] into left [${left.join(',')}] and right [${right.join(',')}].`, {depth});

    push(4, `Recursively call mergeSort on the left half.`, {depth});
    const sortedLeft = divide(leftNode, depth+1, order*2);
    setArr(leftNode.id, sortedLeft); setStatus(leftNode.id,'sorted-sub');

    push(5, `Recursively call mergeSort on the right half.`, {depth});
    const sortedRight = divide(rightNode, depth+1, order*2+1);
    setArr(rightNode.id, sortedRight); setStatus(rightNode.id,'sorted-sub');

    setStatus(node.id,'merging');
    push(6, `Conquer step: merge the two sorted halves [${sortedLeft.join(',')}] and [${sortedRight.join(',')}].`, {depth});
    const merged = merge(sortedLeft, sortedRight, depth);
    setArr(node.id, merged); setStatus(node.id,'merged');
    push(19, `Merged result for this segment: [${merged.join(',')}].`, {depth});
    return merged;
  }

  function merge(left,right,depth){
    let i=0,j=0; const result=[];
    while(i<left.length && j<right.length){
      push(12, `Compare left[${i}]=${left[i]} with right[${j}]=${right[j]}.`, {depth,i,j});
      if(left[i]<=right[j]){ result.push(left[i]); i++; }
      else { result.push(right[j]); j++; }
    }
    while(i<left.length){ result.push(left[i]); i++; }
    while(j<right.length){ result.push(right[j]); j++; }
    return result;
  }

  const finalArr = divide(root,0,0);
  setArr(root.id, finalArr); setStatus(root.id,'merged');
  push(19, `mergeSort complete. Final sorted array: [${finalArr.join(',')}].`, {});
  return steps;
}

/* ---------------- Factorial (call stack) ---------------- */
const CODE_FACTORIAL = [
"int factorial(int n) {",
"    if (n <= 1) {",
"        return 1;",
"    }",
"    return n * factorial(n - 1);",
"}"
];
function factorialTrace(n){
  const steps=[]; const frames=[]; let fid=0;
  const push=(line,desc,vars)=>steps.push({line,desc,vars:vars||{},frames:frames.map(f=>Object.assign({},f))});
  function fact(k){
    const id=fid++;
    frames.push({id,fn:'factorial',n:k,status:'calling',result:null});
    push(1, `Call factorial(${k}) — a new stack frame is pushed.`, {n:k});
    push(2, `Check base case: is ${k} <= 1?`, {n:k});
    if(k<=1){
      frames[frames.length-1].status='base'; frames[frames.length-1].result=1;
      push(3, `Yes → factorial(${k}) returns 1 immediately.`, {n:k});
      const val = 1;
      frames.pop();
      return val;
    }
    frames[frames.length-1].status='waiting';
    push(5, `No → need factorial(${k-1}) before finishing factorial(${k}).`, {n:k});
    const sub = fact(k-1);
    const val = k*sub;
    frames[frames.length-1].status='done'; frames[frames.length-1].result=val;
    push(5, `factorial(${k}) = ${k} * factorial(${k-1}) = ${k} * ${sub} = ${val}.`, {n:k,val});
    frames.pop();
    return val;
  }
  push(1, `Starting: compute factorial(${n}).`, {n});
  const res = fact(n);
  push(5, `factorial(${n}) = ${res}. All stack frames popped.`, {n,res});
  return steps;
}

/* ---------------- Fibonacci (recursion tree) ---------------- */
const CODE_FIBONACCI = [
"int fibonacci(int n) {",
"    if (n <= 1) {",
"        return n;",
"    }",
"    return fibonacci(n - 1) + fibonacci(n - 2);",
"}"
];
function fibonacciTrace(n){
  const steps=[]; let nodeId=0; const nodes=[];
  const push=(line,desc,vars)=>steps.push({line,desc,vars:vars||{},nodes:nodes.map(x=>Object.assign({},x))});
  function newNode(parentId,depth,order,label,status){ const nd={id:nodeId++,parentId,depth,order,label,status}; nodes.push(nd); return nd; }
  function setStatus(id,status,label){ const nd=nodes.find(x=>x.id===id); if(nd){ nd.status=status; if(label!==undefined) nd.label=label; } }

  push(1, `Computing fibonacci(${n}) recursively.`, {n});
  function fib(k, parentId, depth, order){
    const node = newNode(parentId, depth, order, `fib(${k})`, 'calling');
    push(1, `Call fibonacci(${k}).`, {k,depth});
    push(2, `Check base case: is ${k} <= 1?`, {k,depth});
    if(k<=1){
      setStatus(node.id,'base',`fib(${k})=${k}`);
      push(3, `Yes → fibonacci(${k}) returns ${k}.`, {k,depth});
      return k;
    }
    push(5, `No → need fibonacci(${k-1}) and fibonacci(${k-2}).`, {k,depth});
    const left = fib(k-1, node.id, depth+1, order*2);
    const right = fib(k-2, node.id, depth+1, order*2+1);
    const val = left+right;
    setStatus(node.id,'done',`fib(${k})=${val}`);
    push(5, `fibonacci(${k}) = ${left} + ${right} = ${val}.`, {k,depth,val});
    return val;
  }
  const result = fib(n,null,0,0);
  push(5, `fibonacci(${n}) = ${result}. Recursion tree complete.`, {result});
  return steps;
}

/* ---------------- Stack (array-backed) ---------------- */
const CODE_STACK = [
"void push(int val) {",
"    if (top == CAPACITY - 1) {",
"        printf(\"Stack Overflow\\n\");",
"        return;",
"    }",
"    arr[++top] = val;",
"}",
"",
"int pop() {",
"    if (top == -1) {",
"        printf(\"Stack Underflow\\n\");",
"        return -1;",
"    }",
"    return arr[top--];",
"}",
"",
"int peek() {",
"    return arr[top];",
"}"
];
function stackDemoTrace(){
  const steps=[]; const arr=[]; let top=-1; const CAP=6;
  const push_=(line,desc,vars)=>steps.push({line,desc,vars:vars||{},struct:{kind:'stack',arr:arr.slice(),top,cap:CAP}});
  function doPush(v){
    push_(2, `push(${v}): check for overflow — top(${top}) == capacity-1(${CAP-1})?`, {v,top});
    top++; arr[top]=v;
    push_(6, `Not full → place ${v} at arr[${top}], top becomes ${top}.`, {v,top});
  }
  function doPop(){
    push_(10, `pop(): check for underflow — top == -1? (top=${top})`, {top});
    const v = arr[top];
    push_(14, `Not empty → return arr[${top}]=${v}, then decrement top.`, {v,top});
    arr.length=top; top--;
  }
  push_(1, 'Stack initialized: top = -1 (empty).', {top});
  doPush(10); doPush(20); doPush(30);
  push_(18, `peek(): top element is arr[${top}]=${arr[top]} — no removal.`, {top,peek:arr[top]});
  doPop();
  doPush(40);
  push_(1, 'Demo sequence complete.', {top});
  return steps;
}

/* ---------------- Queue (array-backed, linear) ---------------- */
const CODE_QUEUE = [
"void enqueue(int val) {",
"    if (rear == CAPACITY - 1) {",
"        printf(\"Queue Full\\n\");",
"        return;",
"    }",
"    if (front == -1) front = 0;",
"    arr[++rear] = val;",
"}",
"",
"int dequeue() {",
"    if (front == -1 || front > rear) {",
"        printf(\"Queue Empty\\n\");",
"        return -1;",
"    }",
"    int val = arr[front];",
"    front++;",
"    return val;",
"}"
];
function queueDemoTrace(){
  const steps=[]; const CAP=6; const arr=new Array(CAP).fill(null); let front=-1, rear=-1;
  const push_=(line,desc,vars)=>steps.push({line,desc,vars:vars||{},struct:{kind:'queue',arr:arr.slice(),front,rear,cap:CAP}});
  function enqueue(v){
    push_(2, `enqueue(${v}): check for overflow — rear(${rear}) == capacity-1(${CAP-1})?`, {v,front,rear});
    if(front===-1) front=0;
    rear++; arr[rear]=v;
    push_(7, `Not full → place ${v} at arr[${rear}], rear becomes ${rear}.`, {v,front,rear});
  }
  function dequeue(){
    push_(11, `dequeue(): check for underflow — is queue empty?`, {front,rear});
    const v = arr[front];
    push_(15, `Not empty → read arr[${front}]=${v}.`, {front,rear,v});
    arr[front]=null; front++;
    push_(16, `Move front forward: front is now ${front}.`, {front,rear,v});
    if(front>rear) { front=-1; rear=-1; }
  }
  push_(1, 'Queue initialized: front = rear = -1 (empty).', {front,rear});
  enqueue(10); enqueue(20); enqueue(30);
  dequeue();
  enqueue(40);
  push_(1, 'Demo sequence complete.', {front,rear});
  return steps;
}

/* ---------------- Singly Linked List ---------------- */
const CODE_LINKEDLIST = [
"void insertEnd(int val) {",
"    Node* node = createNode(val);",
"    if (head == NULL) {",
"        head = node;",
"        return;",
"    }",
"    Node* temp = head;",
"    while (temp->next != NULL) {",
"        temp = temp->next;",
"    }",
"    temp->next = node;",
"}",
"",
"void insertFront(int val) {",
"    Node* node = createNode(val);",
"    node->next = head;",
"    head = node;",
"}",
"",
"void deleteNode(int val) {",
"    Node *temp = head, *prev = NULL;",
"    while (temp != NULL && temp->val != val) {",
"        prev = temp;",
"        temp = temp->next;",
"    }",
"    if (temp == NULL) return;",
"    if (prev == NULL) head = temp->next;",
"    else prev->next = temp->next;",
"    free(temp);",
"}"
];
function linkedListDemoTrace(){
  const steps=[]; let nodes=[]; let headId=null; let nid=0;
  const push_=(line,desc,vars)=>steps.push({line,desc,vars:vars||{},struct:{kind:'ll',nodes:nodes.map(n=>Object.assign({},n)),headId}});
  function insertEnd(v){
    push_(2, `insertEnd(${v}): create a new node holding ${v}.`, {v});
    const node={id:nid++,val:v,nextId:null};
    nodes.push(node);
    if(headId===null){
      headId=node.id;
      push_(4, `List was empty → new node becomes head.`, {v});
    } else {
      push_(7, `Traverse from head to find the last node.`, {v});
      let cur=nodes.find(n=>n.id===headId);
      while(cur.nextId!==null){ cur=nodes.find(n=>n.id===cur.nextId); }
      cur.nextId=node.id;
      push_(10, `Link the last node's next pointer to the new node (${v}).`, {v});
    }
  }
  function insertFront(v){
    push_(15, `insertFront(${v}): create a new node holding ${v}.`, {v});
    const node={id:nid++,val:v,nextId:headId};
    nodes.push(node);
    headId=node.id;
    push_(17, `New node's next points to old head; it becomes the new head.`, {v});
  }
  function deleteVal(v){
    push_(21, `deleteNode(${v}): traverse the list looking for value ${v}.`, {v});
    let prev=null; let cur = nodes.find(n=>n.id===headId);
    while(cur && cur.val!==v){ prev=cur; cur=nodes.find(n=>n.id===cur.nextId); }
    if(cur){
      if(prev===null){ headId=cur.nextId; }
      else { prev.nextId=cur.nextId; }
      nodes = nodes.filter(n=>n.id!==cur.id);
      push_(28, `Found node with value ${v} → unlink it and free its memory.`, {v});
    }
  }
  push_(1, 'Linked list initialized: head = NULL (empty list).', {});
  insertEnd(10); insertEnd(20); insertEnd(30);
  insertFront(5);
  deleteVal(20);
  push_(1, 'Demo sequence complete.', {});
  return steps;
}
/* =====================================================================
   Part 3: Algorithm registry — wires C source + trace generator +
   visualization type + default inputs together for the engine.
   ===================================================================== */

/* ---------------- GCD (Euclidean recursion) ---------------- */
const CODE_GCD = [
"int gcd(int a, int b) {",
"    if (b == 0) {",
"        return a;",
"    }",
"    return gcd(b, a % b);",
"}"
];
function gcdTrace(a,b){
  const steps=[]; const frames=[]; let fid=0;
  const push=(line,desc,vars)=>steps.push({line,desc,vars:vars||{},frames:frames.map(f=>Object.assign({},f))});
  function gcd(x,y){
    const id=fid++;
    frames.push({id,fn:'gcd',n:`${x}, ${y}`,status:'calling',result:null});
    push(1, `Call gcd(${x}, ${y}).`, {x,y});
    push(2, `Check base case: is ${y} == 0?`, {x,y});
    if(y===0){
      frames[frames.length-1].status='base'; frames[frames.length-1].result=x;
      push(3, `Yes → gcd(${x}, ${y}) returns ${x}.`, {x,y});
      frames.pop();
      return x;
    }
    frames[frames.length-1].status='waiting';
    push(5, `No → recurse with gcd(${y}, ${x} % ${y}) = gcd(${y}, ${x % y}).`, {x,y});
    const sub = gcd(y, x % y);
    frames[frames.length-1].status='done'; frames[frames.length-1].result=sub;
    push(5, `gcd(${x}, ${y}) = ${sub}.`, {x,y,result:sub});
    frames.pop();
    return sub;
  }
  push(1, `Starting: compute gcd(${a}, ${b}).`, {a,b});
  const res = gcd(a,b);
  push(5, `gcd(${a}, ${b}) = ${res}.`, {res});
  return steps;
}

/* ---------------- Tower of Hanoi ---------------- */
const CODE_HANOI = [
"void hanoi(int k, char from, char to, char via) {",
"    if (k == 0) {",
"        return;",
"    }",
"    hanoi(k - 1, from, via, to);",
"    printf(\"Move disk %d from %c to %c\\n\", k, from, to);",
"    hanoi(k - 1, via, to, from);",
"}"
];
function hanoiTrace(n){
  const steps=[]; const pegs = {A:[],B:[],C:[]};
  for(let d=n; d>=1; d--) pegs.A.push(d);
  const push=(line,desc,vars)=>steps.push({line,desc,vars:vars||{},pegs:{A:pegs.A.slice(),B:pegs.B.slice(),C:pegs.C.slice()}});
  push(1, `hanoi called: move ${n} disk(s) from A to C, using B as helper.`, {n});
  function hanoi(k, from, to, via){
    push(2, `Base check: is k(${k}) == 0?`, {k,from,to,via});
    if(k===0){ push(3, `Yes → nothing to move, return.`, {k}); return; }
    push(5, `First, move the top ${k-1} disk(s) from ${from} to ${via}.`, {k,from,to,via});
    hanoi(k-1, from, via, to);
    const disk = pegs[from].pop();
    pegs[to].push(disk);
    push(6, `Move disk ${disk} directly from ${from} to ${to}.`, {k,from,to,via});
    push(7, `Finally, move the ${k-1} disk(s) from ${via} on top of disk ${disk} on ${to}.`, {k,from,to,via});
    hanoi(k-1, via, to, from);
  }
  hanoi(n,'A','C','B');
  push(8, `All ${n} disk(s) moved to peg C. Done.`, {n});
  return steps;
}

/* ---------------- Heap Sort ---------------- */
const CODE_HEAP = [
"void heapify(int arr[], int size, int i) {",
"    int largest = i, l = 2*i + 1, r = 2*i + 2;",
"    if (l < size && arr[l] > arr[largest]) largest = l;",
"    if (r < size && arr[r] > arr[largest]) largest = r;",
"    if (largest != i) {",
"        int temp = arr[i]; arr[i] = arr[largest]; arr[largest] = temp;",
"        heapify(arr, size, largest);",
"    }",
"}",
"",
"void heapSort(int arr[], int n) {",
"    for (int i = n/2 - 1; i >= 0; i--) heapify(arr, n, i);",
"    for (int i = n - 1; i > 0; i--) {",
"        int temp = arr[0]; arr[0] = arr[i]; arr[i] = temp;",
"        heapify(arr, i, 0);",
"    }",
"}"
];
function heapSortTrace(arrIn){
  const arr = arrIn.slice(); const n=arr.length; const steps=[];
  const push=(line,desc,hi,vars)=>steps.push({line,desc,arr:arr.slice(),highlights:hi||{},vars:vars||{}});
  const finalized = new Set();
  const markFinal=(extra)=>{ const hi={}; finalized.forEach(k=>hi[k]='sorted'); return Object.assign(hi,extra); };
  function heapify(size, i){
    let largest=i, l=2*i+1, r=2*i+2;
    push(2, `heapify at index ${i} (heap size ${size}): find the largest of node and its children.`, markFinal({[i]:'current'}), {i,size});
    if(l<size){
      push(3, `Compare left child arr[${l}]=${arr[l]} with arr[${largest}]=${arr[largest]}.`, markFinal({[i]:'current',[l]:'compare'}), {i,l});
      if(arr[l]>arr[largest]) largest=l;
    }
    if(r<size){
      push(4, `Compare right child arr[${r}]=${arr[r]} with arr[${largest}]=${arr[largest]}.`, markFinal({[i]:'current',[r]:'compare'}), {i,r});
      if(arr[r]>arr[largest]) largest=r;
    }
    if(largest!==i){
      [arr[i],arr[largest]]=[arr[largest],arr[i]];
      push(6, `arr[${largest}] is the largest → swap with arr[${i}].`, markFinal({[i]:'swap',[largest]:'swap'}), {i,largest});
      heapify(size,largest);
    }
  }
  push(12, `Phase 1: build a max heap from all ${n} elements.`, {n});
  for(let i=Math.floor(n/2)-1;i>=0;i--) heapify(n,i);
  push(13, `Max heap built — the largest element now sits at the root (index 0).`, markFinal({0:'current'}), {});
  for(let i=n-1;i>0;i--){
    [arr[0],arr[i]]=[arr[i],arr[0]];
    finalized.add(i);
    push(15, `Swap root arr[0] with arr[${i}] — the current largest moves to its final sorted spot.`, markFinal({0:'swap',[i]:'swap'}), {i});
    heapify(i,0);
  }
  finalized.add(0);
  push(17, 'heapSort complete — array fully sorted.', markFinal({}), {});
  return steps;
}

/* ---------------- Binary Search Tree: insert ---------------- */
const CODE_BST = [
"Node* insert(Node* root, int val) {",
"    if (root == NULL) {",
"        return createNode(val);",
"    }",
"    if (val < root->val) {",
"        root->left = insert(root->left, val);",
"    } else if (val > root->val) {",
"        root->right = insert(root->right, val);",
"    }",
"    return root;",
"}"
];
function bstInsertTrace(values){
  const steps=[]; let nodeId=0; const nodes=[];
  let root=null;
  const push=(line,desc,vars)=>steps.push({line,desc,vars:vars||{},nodes:nodes.map(n=>Object.assign({},n))});
  const markStatus=(id,status)=>{ const n=nodes.find(x=>x.id===id); if(n) n.status=status; };

  function insert(nodeRef, val, depth, order, parentId){
    if(nodeRef===null){
      push(2, `Position is empty (NULL) → this is where ${val} belongs.`, {val});
      const id=nodeId++;
      nodes.push({id,parentId,depth,order,val,label:String(val),status:'base'});
      push(3, `Create a new node holding ${val}.`, {val});
      return {id,val,left:null,right:null};
    }
    markStatus(nodeRef.id,'merging');
    push(5, `Compare ${val} with current node ${nodeRef.val}: is ${val} < ${nodeRef.val}?`, {val, cur:nodeRef.val});
    if(val<nodeRef.val){
      push(6, `Yes → recurse into the left subtree.`, {val});
      nodeRef.left = insert(nodeRef.left, val, depth+1, order*2, nodeRef.id);
    } else if(val>nodeRef.val){
      push(7, `No, ${val} > ${nodeRef.val} → recurse into the right subtree.`, {val});
      nodeRef.right = insert(nodeRef.right, val, depth+1, order*2+1, nodeRef.id);
    } else {
      push(9, `${val} already exists in the tree — nothing to insert.`, {val});
    }
    markStatus(nodeRef.id,'done');
    return nodeRef;
  }
  push(1, `Insert values [${values.join(', ')}] one at a time into the BST.`, {});
  values.forEach(v=>{
    push(1, `insert(root, ${v}) called.`, {val:v});
    root = insert(root, v, 0, 0, null);
  });
  push(10, 'All insertions complete — final binary search tree shown.', {});
  return steps;
}

var DEFAULT_CUSTOM_SAMPLE = [
"int main() {",
"    int arr[6] = {5, 3, 8, 1, 9, 2};",
"    int i, j, temp, n;",
"    n = 6;",
"    for (i = 0; i < n - 1; i++) {",
"        for (j = 0; j < n - i - 1; j++) {",
"            if (arr[j] > arr[j + 1]) {",
"                temp = arr[j];",
"                arr[j] = arr[j + 1];",
"                arr[j + 1] = temp;",
"            }",
"        }",
"    }",
"    for (i = 0; i < n; i++) {",
"        printf(\"%d \", arr[i]);",
"    }",
"    return 0;",
"}"
].join('\n');

var ALGORITHMS = {
  custom: {
    id:'custom', name:'Write Your Own Code', category:'Custom',
    desc:'Paste or write any C program (it needs an int main()). It will be parsed and stepped through line by line, with console output, the live call stack, and any arrays auto-detected and drawn as bars.',
    time:'—', space:'—',
    code: DEFAULT_CUSTOM_SAMPLE.split('\n'), viz:'custom',
    inputs:[]
  },
  bubble: {
    id:'bubble', name:'Bubble Sort', category:'Sorting',
    desc:'Repeatedly steps through the array, comparing adjacent elements and swapping them if they are in the wrong order, until no swaps are needed.',
    time:'O(n²)', space:'O(1)',
    code: CODE_BUBBLE, viz:'bars',
    inputs:[{key:'array', label:'Array (comma-separated)', type:'array', default:[5,3,8,4,2,7]}],
    trace: (input) => bubbleSortTrace(input.array)
  },
  selection: {
    id:'selection', name:'Selection Sort', category:'Sorting',
    desc:'Divides the array into a sorted and unsorted part, repeatedly selecting the smallest element from the unsorted part and moving it to the end of the sorted part.',
    time:'O(n²)', space:'O(1)',
    code: CODE_SELECTION, viz:'bars',
    inputs:[{key:'array', label:'Array (comma-separated)', type:'array', default:[6,2,9,1,5,4]}],
    trace: (input) => selectionSortTrace(input.array)
  },
  insertion: {
    id:'insertion', name:'Insertion Sort', category:'Sorting',
    desc:'Builds the final sorted array one element at a time, taking each new element and inserting it into its correct position among the already-sorted elements.',
    time:'O(n²)', space:'O(1)',
    code: CODE_INSERTION, viz:'bars',
    inputs:[{key:'array', label:'Array (comma-separated)', type:'array', default:[9,4,7,2,6,3]}],
    trace: (input) => insertionSortTrace(input.array)
  },
  quick: {
    id:'quick', name:'Quick Sort', category:'Sorting',
    desc:'Picks a pivot element and partitions the array so smaller elements land left of the pivot and larger ones land right, then recursively sorts each side.',
    time:'O(n log n) avg', space:'O(log n)',
    code: CODE_QUICK, viz:'bars',
    inputs:[{key:'array', label:'Array (comma-separated)', type:'array', default:[8,3,7,4,9,1,6]}],
    trace: (input) => quickSortTrace(input.array)
  },
  heap: {
    id:'heap', name:'Heap Sort', category:'Sorting',
    desc:'Builds a max-heap from the array so the largest element sits at the root, then repeatedly swaps the root with the last unsorted element and re-heapifies.',
    time:'O(n log n)', space:'O(1)',
    code: CODE_HEAP, viz:'bars',
    inputs:[{key:'array', label:'Array (comma-separated)', type:'array', default:[9,4,7,1,8,3,6]}],
    trace: (input) => heapSortTrace(input.array)
  },
  merge: {
    id:'merge', name:'Merge Sort', category:'Sorting',
    desc:'A true divide-and-conquer algorithm: splits the array in half recursively until pieces of size 1 remain, then merges sorted pieces back together.',
    time:'O(n log n)', space:'O(n)',
    code: CODE_MERGE, viz:'tree-merge',
    inputs:[{key:'array', label:'Array (comma-separated)', type:'array', default:[6,3,8,2,9,4]}],
    trace: (input) => mergeSortTrace(input.array)
  },
  linear: {
    id:'linear', name:'Linear Search', category:'Searching',
    desc:'Scans the array from left to right, checking each element against the target until a match is found or the array is exhausted.',
    time:'O(n)', space:'O(1)',
    code: CODE_LINEAR, viz:'bars',
    inputs:[
      {key:'array', label:'Array (comma-separated)', type:'array', default:[4,8,1,9,3,6]},
      {key:'target', label:'Target value', type:'number', default:9, min:-999, max:999}
    ],
    trace: (input) => linearSearchTrace(input.array, input.target)
  },
  binary: {
    id:'binary', name:'Binary Search', category:'Searching',
    desc:'Repeatedly halves a sorted array, comparing the middle element to the target to decide which half to keep searching.',
    time:'O(log n)', space:'O(1)',
    code: CODE_BINARY, viz:'bars',
    inputs:[
      {key:'array', label:'Array — will be sorted first', type:'array', default:[4,8,1,9,3,6,2]},
      {key:'target', label:'Target value', type:'number', default:8, min:-999, max:999}
    ],
    trace: (input) => binarySearchTrace(input.array, input.target)
  },
  factorial: {
    id:'factorial', name:'Factorial (Recursion)', category:'Recursion',
    desc:'A recursive function that calls itself with a smaller input until it hits a base case, then multiplies results back up the call stack.',
    time:'O(n)', space:'O(n) stack',
    code: CODE_FACTORIAL, viz:'stack-frames',
    inputs:[{key:'n', label:'n', type:'number', default:5, min:0, max:10}],
    trace: (input) => factorialTrace(input.n)
  },
  fibonacci: {
    id:'fibonacci', name:'Fibonacci (Recursion)', category:'Recursion',
    desc:'Computes the nth Fibonacci number via two recursive calls per step, forming a branching recursion tree with overlapping sub-problems.',
    time:'O(2ⁿ)', space:'O(n) stack',
    code: CODE_FIBONACCI, viz:'tree-fib',
    inputs:[{key:'n', label:'n', type:'number', default:5, min:0, max:9}],
    trace: (input) => fibonacciTrace(input.n)
  },
  gcd: {
    id:'gcd', name:'GCD (Euclidean, Recursion)', category:'Recursion',
    desc:'Finds the greatest common divisor by recursively replacing (a, b) with (b, a mod b) until the remainder reaches zero.',
    time:'O(log(min(a,b)))', space:'O(log(min(a,b))) stack',
    code: CODE_GCD, viz:'stack-frames',
    inputs:[{key:'a', label:'a', type:'number', default:48, min:1, max:9999},{key:'b', label:'b', type:'number', default:18, min:0, max:9999}],
    trace: (input) => gcdTrace(input.a, input.b)
  },
  hanoi: {
    id:'hanoi', name:'Tower of Hanoi', category:'Recursion',
    desc:'Moves a stack of disks from peg A to peg C using peg B as a helper, never placing a larger disk on a smaller one — the classic divide-and-conquer puzzle.',
    time:'O(2ⁿ)', space:'O(n) stack',
    code: CODE_HANOI, viz:'hanoi',
    inputs:[{key:'n', label:'Number of disks', type:'number', default:3, min:1, max:6}],
    trace: (input) => hanoiTrace(input.n)
  },
  bst: {
    id:'bst', name:'Binary Search Tree (Insert)', category:'Data Structures',
    desc:'Inserts values one at a time: at each node, go left if the new value is smaller, right if larger, until an empty spot is found — building a tree where left < parent < right everywhere.',
    time:'O(log n) avg / O(n) worst', space:'O(n)',
    code: CODE_BST, viz:'tree-merge',
    inputs:[{key:'array', label:'Values to insert (comma-separated)', type:'array', default:[8,3,10,1,6,14,4,7]}],
    trace: (input) => bstInsertTrace(input.array)
  },
  stack: {
    id:'stack', name:'Stack (Array)', category:'Data Structures',
    desc:'A Last-In-First-Out structure. This demo runs push(10), push(20), push(30), peek(), pop(), push(40) using a fixed-capacity array and a top pointer.',
    time:'O(1) / op', space:'O(n)',
    code: CODE_STACK, viz:'struct-stack',
    inputs:[], trace: () => stackDemoTrace()
  },
  queue: {
    id:'queue', name:'Queue (Array)', category:'Data Structures',
    desc:'A First-In-First-Out structure. This demo runs enqueue(10,20,30), dequeue(), enqueue(40) using a fixed-capacity array with front/rear pointers.',
    time:'O(1) / op', space:'O(n)',
    code: CODE_QUEUE, viz:'struct-queue',
    inputs:[], trace: () => queueDemoTrace()
  },
  linkedlist: {
    id:'linkedlist', name:'Linked List', category:'Data Structures',
    desc:'A chain of nodes linked by pointers. This demo inserts 10, 20, 30 at the end, inserts 5 at the front, then deletes the node holding 20.',
    time:'O(n) / op', space:'O(n)',
    code: CODE_LINKEDLIST, viz:'struct-ll',
    inputs:[], trace: () => linkedListDemoTrace()
  }
};

var CATEGORY_ORDER = ['Custom','Sorting','Searching','Recursion','Data Structures'];
/* =====================================================================
   Part 4: C syntax highlighter, per-visualization renderers, and the
   playback engine that drives code panel + viz panel + controls.
   ===================================================================== */

function escapeXml(s){
  return String(s).replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;');
}

/* ---------------- Tiny C syntax highlighter ---------------- */
function highlightC(line){
  if(line.trim()==='') return '&nbsp;';
  let s = line.replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;');
  const pattern = /("(?:[^"\\]|\\.)*")|(\b(?:int|void|return|if|else|while|for|char|float|double|struct)\b)|(\b(?:printf|scanf|malloc|free|createNode)\b)|(\b(?:Node|MAX|CAPACITY|NULL)\b)|(\b\d+\b)/g;
  s = s.replace(pattern, (m, str, kw, fn, type, num) => {
    if(str) return `<span class="tok-str">${str}</span>`;
    if(kw) return `<span class="tok-kw">${kw}</span>`;
    if(fn) return `<span class="tok-fn">${fn}</span>`;
    if(type) return `<span class="tok-type">${type}</span>`;
    if(num) return `<span class="tok-num">${num}</span>`;
    return m;
  });
  return s;
}

/* ---------------- Renderer: array bars ---------------- */
function renderBars(step, container){
  const arr = step.arr || [];
  if(!arr.length){ container.innerHTML = '<div style="color:var(--muted);font-family:var(--font-code);font-size:13px;">empty array</div>'; return; }
  const max = Math.max(...arr, 1);
  const ptrRoles = {lo:'LO', hi:'HI', mid:'MID', current:'i', min:'MIN', pivot:'PIVOT'};
  let html = '<div class="bars-wrap">';
  arr.forEach((v,i)=>{
    const role = (step.highlights||{})[i];
    const cls = role ? ' '+role : '';
    const heightPx = 46 + (v/max)*220;
    const ptrLabel = role && ptrRoles[role] ? `<span class="ptr">${ptrRoles[role]}</span>` : '';
    html += `<div class="bar${cls}" style="height:${heightPx}px">${ptrLabel}<span class="val">${v}</span><span class="idx">${i}</span></div>`;
  });
  html += '</div>';
  container.innerHTML = html;
}

/* ---------------- Renderer: divide & conquer / recursion tree ---------------- */
function renderTree(nodes, container){
  if(!nodes || !nodes.length){ container.innerHTML = '<div style="color:var(--muted);font-family:var(--font-code);font-size:13px;">no calls yet</div>'; return; }
  const maxDepth = Math.max(...nodes.map(n=>n.depth));
  const width = Math.max(560, Math.pow(2,maxDepth) * 90 + 80);
  const rowH = 88;
  const boxH = 38;
  const height = 30 + (maxDepth+1)*rowH;
  let svg = `<svg class="tree-svg" viewBox="0 0 ${width} ${height}" xmlns="http://www.w3.org/2000/svg">`;
  nodes.forEach(n=>{
    if(n.parentId!==null && n.parentId!==undefined){
      const p = nodes.find(x=>x.id===n.parentId);
      if(p){
        const x1 = (p.order+0.5)/Math.pow(2,p.depth)*width, y1 = 20+p.depth*rowH+boxH;
        const x2 = (n.order+0.5)/Math.pow(2,n.depth)*width, y2 = 20+n.depth*rowH;
        svg += `<line class="tedge" x1="${x1.toFixed(1)}" y1="${y1}" x2="${x2.toFixed(1)}" y2="${y2}"/>`;
      }
    }
  });
  nodes.forEach(n=>{
    const x = (n.order+0.5)/Math.pow(2,n.depth)*width;
    const y = 20+n.depth*rowH;
    const label = escapeXml(n.label||'');
    const boxW = Math.max(58, label.length*7.4+18);
    svg += `<g><rect class="tnode-box ${n.status||''}" x="${(x-boxW/2).toFixed(1)}" y="${y}" width="${boxW.toFixed(1)}" height="${boxH}" rx="9"/>`+
           `<text x="${x.toFixed(1)}" y="${y+boxH/2+5}" text-anchor="middle" font-size="12.5" font-weight="600">${label}</text></g>`;
  });
  svg += '</svg>';
  container.innerHTML = svg;
}

/* ---------------- Renderer: recursion call stack frames ---------------- */
function renderFrames(frames, container){
  if(!frames || !frames.length){ container.innerHTML = '<div style="color:var(--muted);font-family:var(--font-code);font-size:13px;">call stack empty</div>'; return; }
  let html = '<div class="frames-wrap">';
  frames.forEach(f=>{
    html += `<div class="frame ${f.status}"><span class="fn">${f.fn||'factorial'}(${f.n})</span><span class="res">${f.result!==null && f.result!==undefined ? '= '+f.result : '…'}</span></div>`;
  });
  html += '</div>';
  container.innerHTML = html;
}

/* ---------------- Renderer: array-backed stack ---------------- */
function renderStructStack(s, container){
  let html = '<div class="struct-stack">';
  for(let i=0;i<s.cap;i++){
    const filled = i<=s.top;
    const cls = 'slot' + (filled?' filled':'') + (i===s.top?' top-slot':'');
    html += `<div class="${cls}">${filled ? s.arr[i] : ''}</div>`;
  }
  html += '</div>';
  container.innerHTML = html;
}

/* ---------------- Renderer: array-backed queue ---------------- */
function renderStructQueue(s, container){
  let html = '<div class="struct-queue">';
  for(let i=0;i<s.cap;i++){
    const filled = s.arr[i]!==null && s.arr[i]!==undefined;
    let cls = 'slot' + (filled?' filled':'');
    if(i===s.front && s.front!==-1) cls += ' front-slot';
    if(i===s.rear && s.rear!==-1) cls += ' rear-slot';
    html += `<div class="${cls}">${filled ? s.arr[i] : ''}</div>`;
  }
  html += '</div>';
  container.innerHTML = html;
}

/* ---------------- Renderer: singly linked list ---------------- */
function renderStructLL(s, container){
  if(s.headId===null || s.headId===undefined){
    container.innerHTML = '<div><div class="ll-head-label">HEAD</div><div style="color:var(--muted);font-family:var(--font-code);font-size:13px;">NULL (empty list)</div></div>';
    return;
  }
  let html = '<div><div class="ll-head-label">HEAD</div><div class="ll-wrap">';
  let cur = s.nodes.find(n=>n.id===s.headId);
  const seen = new Set();
  while(cur && !seen.has(cur.id)){
    seen.add(cur.id);
    html += `<div class="ll-node"><span class="v">${cur.val}</span><span class="p">${cur.nextId!==null ? '&bull;' : '&empty;'}</span></div>`;
    if(cur.nextId!==null && cur.nextId!==undefined){
      html += `<div class="ll-arrow">&rarr;</div>`;
      cur = s.nodes.find(n=>n.id===cur.nextId);
    } else { cur = null; }
  }
  html += '</div></div>';
  container.innerHTML = html;
}

/* ---------------- Renderer: Tower of Hanoi pegs ---------------- */
function renderHanoi(pegs, container){
  const pegNames = ['A','B','C'];
  let html = '<div class="hanoi-wrap">';
  pegNames.forEach(p=>{
    html += '<div class="hanoi-peg"><div class="hanoi-rod-area">';
    (pegs[p]||[]).forEach(d=>{
      const width = 34 + d*16;
      const hue = (d*47)%360;
      html += `<div class="hanoi-disk" style="width:${width}px;background:hsl(${hue} 65% 55%)">${d}</div>`;
    });
    html += `</div><div class="hanoi-label">Peg <span class="peg-name">${p}</span></div></div>`;
  });
  html += '</div>';
  container.innerHTML = html;
}

/* ---------------- Generic call-tree layout (for the function call map) ----------------
   Unlike the fixed binary layouts used for merge sort / fibonacci / BST, a live
   function-call tree can branch any number of ways (e.g. a loop calling the same
   function 3 times), so positions are recomputed fresh from whatever nodes exist
   at each step using a simple post-order "leaf slot" layout. */
function layoutCallTree(nodes){
  const childrenOf = {};
  nodes.forEach(n=>{
    const key = (n.parentId===null || n.parentId===undefined) ? 'root' : n.parentId;
    (childrenOf[key] = childrenOf[key] || []).push(n);
  });
  const roots = childrenOf['root'] || [];
  let leafCounter = 0;
  const positions = {};
  let maxDepth = 0;
  function visit(node, depth){
    maxDepth = Math.max(maxDepth, depth);
    const kids = childrenOf[node.id] || [];
    if(!kids.length){
      positions[node.id] = { slot: leafCounter, depth };
      leafCounter++;
    } else {
      kids.forEach(k=>visit(k, depth+1));
      const xs = kids.map(k=>positions[k.id].slot);
      positions[node.id] = { slot: (Math.min(...xs)+Math.max(...xs))/2, depth };
    }
  }
  roots.forEach(r=>visit(r,0));
  return { positions, maxDepth, leafCount: Math.max(leafCounter,1) };
}

function renderCallTree(nodes, container){
  if(!nodes || !nodes.length){ container.innerHTML = '<div class="custom-empty-hint">No function calls yet.</div>'; return; }
  const { positions, maxDepth, leafCount } = layoutCallTree(nodes);
  const slotW = 128;
  const width = Math.max(420, leafCount*slotW + 40);
  const rowH = 82, boxH = 38;
  const height = 26 + (maxDepth+1)*rowH;
  let svg = `<svg class="tree-svg" viewBox="0 0 ${width} ${height}" xmlns="http://www.w3.org/2000/svg">`;
  nodes.forEach(n=>{
    if(n.parentId!==null && n.parentId!==undefined){
      const p = nodes.find(x=>x.id===n.parentId);
      if(p){
        const cp = positions[p.id], cc = positions[n.id];
        const x1 = (cp.slot+0.5)*slotW, y1 = 18+cp.depth*rowH+boxH;
        const x2 = (cc.slot+0.5)*slotW, y2 = 18+cc.depth*rowH;
        svg += `<line class="tedge" x1="${x1.toFixed(1)}" y1="${y1}" x2="${x2.toFixed(1)}" y2="${y2}"/>`;
      }
    }
  });
  nodes.forEach(n=>{
    const p = positions[n.id];
    const x = (p.slot+0.5)*slotW, y = 18+p.depth*rowH;
    const label = escapeXml(n.label||n.fnName+'()');
    const boxW = Math.max(76, label.length*7.1+18);
    svg += `<g><rect class="tnode-box ${n.status||''}" x="${(x-boxW/2).toFixed(1)}" y="${y}" width="${boxW.toFixed(1)}" height="${boxH}" rx="9"/>`+
           `<text x="${x.toFixed(1)}" y="${y+boxH/2+5}" text-anchor="middle" font-size="12" font-weight="600">${label}</text></g>`;
  });
  svg += '</svg>';
  container.innerHTML = svg;
}

/* ---------------- Renderer: custom user code (console + stack + arrays) ---------------- */
function renderCustomViz(step, container){
  let html = '<div style="width:100%;display:flex;flex-direction:column;">';

  html += '<div class="custom-viz-section"><div class="strip-label" style="margin-bottom:6px;">CONSOLE OUTPUT</div>';
  html += `<div class="console-box">${step.output ? escapeXml(step.output) : '<span style="opacity:.4">(no output yet)</span>'}</div></div>`;

  html += '<div class="custom-viz-section"><div class="strip-label" style="margin-bottom:6px;">FUNCTION CALL MAP</div>';
  html += `<div id="callTreeHost" style="width:100%;overflow:auto;"></div></div>`;

  html += '<div class="custom-viz-section"><div class="strip-label" style="margin-bottom:6px;">CALL STACK (current locals)</div>';
  if(step.frames && step.frames.length){
    html += '<div class="frames-wrap" style="max-width:100%;">';
    step.frames.forEach((f,idx)=>{
      const isTop = idx===step.frames.length-1;
      const chips = Object.entries(f.locals).map(([k,v])=>`<span class="var-chip" style="margin:2px 0;">${k} = <b>${v}</b></span>`).join('');
      html += `<div class="frame ${isTop?'calling':'done'}" style="height:auto;flex-wrap:wrap;align-items:center;gap:8px;padding:10px 14px;">
        <span class="fn">${f.fn}()</span>
        <span style="display:flex;flex-wrap:wrap;gap:5px;">${chips || '<span style=\"opacity:.4;font-size:11.5px;\">no locals yet</span>'}</span>
      </div>`;
    });
    html += '</div>';
  } else {
    html += '<div style="color:var(--muted);font-family:var(--font-code);font-size:12.5px;">(no active function calls)</div>';
  }
  html += '</div>';

  const arrEntries = Object.entries(step.arrays||{});
  if(arrEntries.length){
    html += '<div class="custom-viz-section"><div class="strip-label" style="margin-bottom:6px;">ARRAY WATCH</div>';
    arrEntries.forEach(([name,arr])=>{
      const max = Math.max(...arr.map(v=>Math.abs(v)), 1);
      html += `<div style="margin-bottom:12px;"><div style="font-family:var(--font-code);font-size:11.5px;color:var(--indigo);margin-bottom:6px;font-weight:600;">${escapeXml(name)}</div><div class="bars-wrap" style="height:110px;gap:5px;padding-top:26px;">`;
      arr.forEach((v,i)=>{
        const h = 18 + (Math.abs(v)/max)*74;
        html += `<div class="bar" style="width:30px;height:${h}px;"><span class="val" style="font-size:10px;top:-18px;">${v}</span><span class="idx" style="font-size:9px;bottom:-18px;">${i}</span></div>`;
      });
      html += '</div></div>';
    });
    html += '</div>';
  }

  html += '</div>';
  container.innerHTML = html;
}

/* ===================================================================
   Engine
   =================================================================== */
var state = { algo:null, steps:[], idx:0, playing:false, timer:null, input:{} };

function buildCodeTable(codeLines){
  const table = document.getElementById('codeTable');
  let html='';
  codeLines.forEach((line,i)=>{
    html += `<tr id="codeline-${i+1}"><td class="ln">${i+1}</td><td class="src">${highlightC(line)}</td></tr>`;
  });
  table.innerHTML = html;
}

function setExecLine(lineSpec){
  document.querySelectorAll('#codeTable tr.exec').forEach(tr=>tr.classList.remove('exec'));
  const lines = Array.isArray(lineSpec) ? lineSpec : [lineSpec];
  let firstEl=null;
  lines.forEach(ln=>{
    const tr = document.getElementById('codeline-'+ln);
    if(tr){ tr.classList.add('exec'); if(!firstEl) firstEl=tr; }
  });
  if(firstEl && firstEl.scrollIntoView){
    firstEl.scrollIntoView({block:'center', behavior:'smooth'});
  }
  document.getElementById('pcChip').textContent = 'LINE ' + lines.join(', ');
}

function renderStep(i){
  const steps = state.steps;
  if(!steps || !steps.length) return;
  i = Math.max(0, Math.min(steps.length-1, i));
  state.idx = i;
  const step = steps[i];
  setExecLine(step.line);
  document.getElementById('narrationText').textContent = step.desc;
  document.getElementById('stepChip').textContent = 'STEP ' + (i+1);
  document.getElementById('stepCounter').textContent = `Step ${i+1} / ${steps.length}`;

  const varsWrap = document.getElementById('varsChips');
  const entries = Object.entries(step.vars||{});
  varsWrap.innerHTML = entries.length
    ? entries.map(([k,v]) => `<span class="var-chip">${k} = <b>${v}</b></span>`).join('')
    : '<span class="var-chip" style="opacity:.5">&mdash;</span>';

  const vizBody = document.getElementById('vizBody');
  const viz = state.algo.viz;
  if(viz==='bars') renderBars(step, vizBody);
  else if(viz==='tree-merge' || viz==='tree-fib') renderTree(step.nodes, vizBody);
  else if(viz==='stack-frames') renderFrames(step.frames, vizBody);
  else if(viz==='struct-stack') renderStructStack(step.struct, vizBody);
  else if(viz==='struct-queue') renderStructQueue(step.struct, vizBody);
  else if(viz==='struct-ll') renderStructLL(step.struct, vizBody);
  else if(viz==='hanoi') renderHanoi(step.pegs, vizBody);
  else if(viz==='custom') renderCustomViz(step, vizBody);

  document.getElementById('btnPrev').disabled = i===0;
  document.getElementById('btnNext').disabled = i===steps.length-1;
}

function updatePlayIcon(){
  document.getElementById('playIcon').innerHTML = state.playing
    ? '<path d="M6 5h4v14H6zM14 5h4v14h-4z"/>'
    : '<path d="M8 5v14l11-7z"/>';
}
function pause(){ state.playing=false; clearTimeout(state.timer); updatePlayIcon(); }
function tick(){
  if(!state.playing) return;
  if(state.idx>=state.steps.length-1){ pause(); return; }
  renderStep(state.idx+1);
  const speed = Number(document.getElementById('speedRange').value);
  const delay = 1300 - speed*220;
  state.timer = setTimeout(tick, delay);
}
function play(){
  if(state.idx>=state.steps.length-1) renderStep(0);
  state.playing = true;
  updatePlayIcon();
  tick();
}

function buildIOControls(algo){
  const wrap = document.getElementById('ioWrap');
  if(algo.id === 'custom'){
    wrap.innerHTML = `
      <div class="io-field"><label>stdin for scanf (comma-separated)</label><input id="customStdin" value=""></div>
      <button class="io-apply" id="customRunBtn">Run My Code &#9654;</button>
      <button class="io-apply" id="customEditBtn" style="background:var(--panel-2);color:var(--ink-soft);border:1px solid var(--border);">Edit Code</button>
      <span class="io-error" id="ioError"></span>
    `;
    document.getElementById('customRunBtn').onclick = runCustomCode;
    document.getElementById('customEditBtn').onclick = enterCustomEditMode;
    return;
  }
  let html='';
  algo.inputs.forEach(inp=>{
    const val = inp.type==='array' ? inp.default.join(', ') : inp.default;
    html += `<div class="io-field"><label>${inp.label}</label><input id="input-${inp.key}" value="${val}"></div>`;
  });
  html += `<button class="io-apply" id="ioApplyBtn">Run &#9654;</button><span class="io-error" id="ioError"></span>`;
  wrap.innerHTML = html;
  document.getElementById('ioApplyBtn').onclick = () => applyInputAndRun(algo);
}

function applyInputAndRun(algo){
  const errEl = document.getElementById('ioError');
  errEl.textContent='';
  const input = {};
  try{
    algo.inputs.forEach(inp=>{
      const raw = document.getElementById('input-'+inp.key).value;
      if(inp.type==='array'){
        const arr = raw.split(',').map(s=>s.trim()).filter(s=>s.length).map(Number);
        if(!arr.length || arr.some(v=>isNaN(v))) throw new Error('Enter valid comma-separated numbers.');
        if(arr.length>12) throw new Error('Use 12 numbers or fewer for a clear visualization.');
        input[inp.key]=arr;
      } else if(inp.type==='number'){
        const num = Number(raw);
        if(isNaN(num)) throw new Error('Enter a valid number.');
        const lo = inp.min!==undefined ? inp.min : 0;
        const hi = inp.max!==undefined ? inp.max : 999999;
        if(num<lo || num>hi) throw new Error(`Use a value between ${lo} and ${hi} to keep the visualization readable.`);
        input[inp.key]=num;
      }
    });
  } catch(e){ errEl.textContent = e.message; return; }
  runAlgorithm(algo, input);
}

function runAlgorithm(algo, input){
  state.algo = algo;
  state.input = input;
  state.steps = algo.trace(input);
  pause();
  renderStep(0);
}

var customSource = DEFAULT_CUSTOM_SAMPLE;

function loadAlgorithm(id){
  const algo = ALGORITHMS[id];
  document.getElementById('categoryPill').textContent = algo.category.toUpperCase();
  document.getElementById('topTitle').textContent = algo.name;
  document.getElementById('topDesc').textContent = algo.desc;
  document.getElementById('badgeTime').textContent = 'Time: '+algo.time;
  document.getElementById('badgeSpace').textContent = 'Space: '+algo.space;
  document.querySelectorAll('.nav-item').forEach(el=>el.classList.toggle('active', el.dataset.id===id));
  buildIOControls(algo);

  if(id === 'custom'){
    state.algo = algo;
    enterCustomEditMode();
    return;
  }

  document.getElementById('customCodeArea').style.display = 'none';
  document.getElementById('codeTable').style.display = '';
  buildCodeTable(algo.code);
  const defaultInput = {};
  algo.inputs.forEach(inp=> defaultInput[inp.key] = inp.type==='array' ? inp.default.slice() : inp.default);
  runAlgorithm(algo, defaultInput);
}

function enterCustomEditMode(){
  document.getElementById('codeTable').style.display = 'none';
  const ta = document.getElementById('customCodeArea');
  ta.style.display = 'block';
  ta.value = customSource;
  document.getElementById('pcChip').textContent = 'EDITING';
  document.getElementById('vizBody').innerHTML =
    '<div class="custom-empty-hint">Write or paste a C program with an <b>int main()</b> function, add any scanf input values, then click "Run My Code ▶".</div>';
  document.getElementById('narrationText').textContent = 'Ready to run your code.';
  document.getElementById('varsChips').innerHTML = '<span class="var-chip" style="opacity:.5">&mdash;</span>';
  document.getElementById('stepCounter').textContent = 'Step 0 / 0';
  document.getElementById('stepChip').textContent = 'STEP 0';
  const errEl = document.getElementById('ioError');
  if(errEl){ errEl.textContent=''; errEl.classList.remove('has-error'); }
  pause();
  state.steps = [];
  state.idx = 0;
}

function runCustomCode(){
  customSource = document.getElementById('customCodeArea').value;
  const stdinRaw = document.getElementById('customStdin') ? document.getElementById('customStdin').value : '';
  const stdin = stdinRaw.split(',').map(s=>s.trim()).filter(s=>s.length).map(Number);
  const errEl = document.getElementById('ioError');
  errEl.textContent=''; errEl.classList.remove('has-error');

  const res = runC(customSource, {stdin});
  const lines = customSource.split('\n');
  buildCodeTable(lines);
  document.getElementById('customCodeArea').style.display = 'none';
  document.getElementById('codeTable').style.display = '';

  if(!res.ok){
    errEl.textContent = 'Error: ' + res.error + (res.line ? ' (line '+res.line+')' : '');
    errEl.classList.add('has-error');
    document.getElementById('narrationText').textContent = 'Stopped with an error — see the message above the controls, then click "Edit Code" to fix it.';
    document.getElementById('varsChips').innerHTML = '<span class="var-chip" style="opacity:.5">&mdash;</span>';
    document.getElementById('stepCounter').textContent = 'Step 0 / 0';
    if(res.line){
      setExecLine(res.line);
      document.querySelectorAll('#codeTable tr.exec').forEach(tr=>tr.classList.add('exec-error'));
    }
    document.getElementById('vizBody').innerHTML = res.output
      ? `<div class="console-box" style="width:100%;">${escapeXml(res.output)}</div>`
      : '<div class="custom-empty-hint">No output was produced before the error.</div>';
    state.steps = [];
    return;
  }

  state.algo = { id:'custom', viz:'custom' };
  state.steps = res.steps;
  pause();
  renderStep(0);
}

function buildNav(){
  const wrap = document.getElementById('navGroups');
  let html='';
  CATEGORY_ORDER.forEach(cat=>{
    html += `<div class="nav-group"><div class="nav-group-label">${cat}</div>`;
    Object.values(ALGORITHMS).filter(a=>a.category===cat).forEach(a=>{
      html += `<button class="nav-item" data-id="${a.id}"><span class="dot"></span>${a.name}</button>`;
    });
    html += '</div>';
  });
  wrap.innerHTML = html;
  document.querySelectorAll('.nav-item').forEach(btn=>{
    btn.onclick = () => loadAlgorithm(btn.dataset.id);
  });
}

document.addEventListener('DOMContentLoaded', () => {
  buildNav();
  document.getElementById('btnReset').onclick = () => { pause(); renderStep(0); };
  document.getElementById('btnPrev').onclick = () => { pause(); renderStep(state.idx-1); };
  document.getElementById('btnNext').onclick = () => { pause(); renderStep(state.idx+1); };
  document.getElementById('btnPlay').onclick = () => { state.playing ? pause() : play(); };
  loadAlgorithm('bubble');
});
