(self.webpackChunkant_design_pro=self.webpackChunkant_design_pro||[]).push([[462],{33599:function(de){de.exports={userAccess:"userAccess___1UdBc","detail-wrap":"detail-wrap___2mcEh","tabs-wrap":"tabs-wrap___3PPX8","content-wrap":"content-wrap___2U6yp",visitedData:"visitedData___1KcWz",visitedCount:"visitedCount___Z0bCy"}},99210:function(){},26410:function(de,V,t){"use strict";t.r(V),t.d(V,{default:function(){return ne}});var p=t(11849),b=t(3182),Q=t(57663),a=t(71577),Y=t(34792),K=t(48086),k=t(86582),R=t(2824),B=t(94043),_=t.n(B),y=t(67294),Ze=t(33599),q=t.n(Ze),ee=t(24480),le=t(84674),pe=t(84437),j=t(6650),be=t(13062),I=t(71230),Te=t(89032),L=t(15746),o=t(13254),i=t(14277),g=t(22385),P=t(61580),e=t(90387),n=t(9536),m=t(69610),f=t(54941),E=t(81306),x=t(19809),w=t(69886),r=t(85893),te=w.G2.getEngine("canvas"),ue=function(ce){(0,E.Z)(c,ce);var s=(0,x.Z)(c);function c(d){var l;return(0,m.Z)(this,c),l=s.call(this,d),l.state={chart:null},l}return(0,f.Z)(c,[{key:"componentDidMount",value:function(){var l=this,C=new w.by("ActionPie".concat(this.props.id),{autoFit:!0,height:250,appendPadding:10,data:this.props.types,angleField:"value",colorField:"type",radius:.75,tooltip:{formatter:function(S){var U=S.value,D=S.type,re=l.props.types.map(function(F){return F.value}).reduce(function(F,ye){return F+ye},0);return{name:D,value:(U/re*100).toFixed(2)+"%"}}},label:{type:"spider",labelHeight:28,formatter:function(S,U){var D=new te.Group({});return D.addShape({type:"circle",attrs:{x:0,y:0,width:40,height:50,r:5,fill:U.color}}),D.addShape({type:"text",attrs:{x:10,y:6,text:"".concat(S.type),fill:U.color}}),D.addShape({type:"text",attrs:{x:0,y:20,text:"".concat(ee.Z.bigNumberTransform(S.value),"\u6B21 "),fill:U.color,fontWeight:700}}),D}},interactions:[{type:"element-selected"},{type:"element-active"}]});C.update({theme:{styleSheet:{brandColor:"#025DF4",paletteQualitative10:["#025DF4","#db6bcf","#2498D1","#BBBDE6","#4045B2","#21A97A","#FF745A","#007E99","#FFA8A8","#2391FF"],paletteQualitative20:["#025DF4","#DB6BCF","#2498D1","#BBBDE6","#4045B2","#21A97A","#FF745A","#007E99","#FFA8A8","#2391FF","#FFC328","#A0DC2C","#946DFF","#626681","#EB4185","#CD8150","#36BCCB","#327039","#803488","#83BC99"]}}}),C.render(),this.setState({chart:C})}},{key:"render",value:function(){return(0,r.jsx)("div",{id:"ActionPie"+this.props.id})}}],[{key:"getDerivedStateFromProps",value:function(l,C){var v=C.chart;return l.refreshUid==="open"&&v&&v.changeData(l.types),l}}]),c}(y.Component),oe=ue,H=t(39004),ae=function(s){var c=(0,y.useCallback)(function(){var d=new H.kL({container:"RecentlyLine".concat(s.id),autoFit:!0,height:200});d.data(s.ranges),d.scale({value:{min:0,nice:!1},year:{range:[0,1]}}),d.tooltip({showNil:!1,showCrosshairs:!1,title:function(C,v){return"".concat(C,"\uFF1A").concat(v.value," \u6B21")},itemTpl:`<div class="g2-tooltip">
  <div class="g2-tooltip-title">Language</div>
  <ul class="g2-tooltip-list">
    <li class="g2-tooltip-list-item">
      <span class="g2-tooltip-marker"></span>
      <span class="g2-tooltip-name">a</span>:<span class="g2-tooltip-value">70</span>
    </li>
    <li class="g2-tooltip-list-item">
      <span class="g2-tooltip-marker"></span>
      <span class="g2-tooltip-name">b</span>:<span class="g2-tooltip-value">50</span>
    </li>
  </ul>
</div>`}),d.axis("value",{label:{}}),d.area().position("year*value"),d.line().position("year*value"),d.render()},[]);return(0,y.useEffect)(function(){c()},[c]),(0,r.jsx)("div",{id:"RecentlyLine"+s.id})},W=ae,ve=function(s){return(0,y.useEffect)(function(){w.G2.registerShape("point","custom-point",{draw:function(l,C){var v={x:l.x,y:l.y},S=C.addGroup();return S.addShape("circle",{name:"outer-point",attrs:{x:v.x,y:v.y,fill:l.color||"red",opacity:.5,r:6}}),S.addShape("circle",{name:"inner-point",attrs:{x:v.x,y:v.y,fill:l.color||"red",opacity:1,r:2}}),S}}),(0,H.w_)("custom-marker-interaction",{start:[{trigger:"tooltip:show",action:"custom-marker-action:active"}],end:[{trigger:"tooltip:hide",action:"custom-marker-action:reset"}]});var c=new w.x1("24hours".concat(s.id),{height:250,autoFit:!0,data:s.data,xField:"year",yField:"value",label:{},point:{size:5,shape:"custom-point",style:{fill:"white",stroke:"#5B8FF9",lineWidth:2}},tooltip:{showMarkers:!1,formatter:function(l){return{name:"\u8BBF\u95EE\u6B21\u6570",value:l.value+"\u6B21"}}},state:{active:{style:{shadowBlur:4,stroke:"#000",fill:"red"}}},interactions:[{type:"custom-marker-interaction"}]});c.render()},[]),(0,r.jsx)("div",{id:"24hours"+s.id})},me=ve,fe=[{label:"\u7ECF\u5E38\u8BBF\u95EE\u7684\u6570\u636E",value:"higher"},{label:"\u4E0D\u5E38\u8BBF\u95EE\u7684\u6570\u636E",value:"lower"}],A=function(s){var c,d,l,C,v,S=(0,y.useState)(fe[0].value),U=(0,R.Z)(S,2),D=U[0],re=U[1];return(0,r.jsxs)(I.Z,{className:s.style["detail-wrap"],children:[(0,r.jsxs)(L.Z,{span:8,className:s.style["tabs-wrap"],children:[(0,r.jsx)(n.Z,{block:!0,options:fe,value:D,onChange:re}),(0,r.jsx)("div",{className:s.style["content-wrap"],children:((c=s.details)===null||c===void 0||(d=c.higherOrLower)===null||d===void 0?void 0:d[D].length)>0?(l=s.details)===null||l===void 0||(C=l.higherOrLower)===null||C===void 0||(v=C[D])===null||v===void 0?void 0:v.map(function(F){return(0,r.jsxs)("div",{className:"flex-x-sb",children:[(0,r.jsx)(P.Z,{title:F.visitedData,children:(0,r.jsx)("span",{id:"visitedData",className:s.style.visitedData,children:F.visitedData})}),(0,r.jsxs)("span",{className:s.style.visitedCount,children:[F.visitedCount," \u6B21"]})]},F.visitedData)}):(0,r.jsx)(i.Z,{})})]}),(0,r.jsxs)(L.Z,{span:16,className:"pl-10 pr-20",children:[(0,r.jsxs)(I.Z,{children:[(0,r.jsxs)(L.Z,{span:12,className:"text-c",children:[(0,r.jsx)("h3",{children:"\u64CD\u4F5C\u884C\u4E3A\u5206\u5E03"}),(0,r.jsx)(oe,{types:s.details.types,id:s.id,refreshUid:s.refreshUid})]}),(0,r.jsxs)(L.Z,{span:12,className:"text-c",children:[(0,r.jsx)("h3",{children:"\u8FD1\u4E03\u5929\u8BBF\u95EE\u6570\u636E\u7EDF\u8BA1"}),(0,r.jsx)(W,{ranges:s.details.ranges,id:s.id})]})]}),(0,r.jsxs)("div",{className:"text-c mt-15 mb-20",children:[(0,r.jsx)("h3",{children:"\u8BBF\u95EE\u65F6\u6BB5\u5206\u5E03"}),(0,r.jsx)(me,{data:s.details.intervals,id:s.id})]})]})]})},ge=A,M=function(){var s=(0,y.useState)(),c=(0,R.Z)(s,2),d=c[0],l=c[1],C=(0,y.useState)(""),v=(0,R.Z)(C,2),S=v[0],U=v[1],D=(0,y.useState)(""),re=(0,R.Z)(D,2),F=re[0],ye=re[1],De=(0,y.useState)(0),Oe=(0,R.Z)(De,2),Ue=Oe[0],Fe=Oe[1],je=(0,y.useState)({pageSize:10,pageNo:1}),Ae=(0,R.Z)(je,2),h=Ae[0],T=Ae[1],J=(0,y.useState)({types:[],intervals:[],ranges:[],higherOrLower:{higher:[],lower:[]}}),he=(0,R.Z)(J,2),Ce=he[0],Se=he[1],Re=[{title:"\u7528\u6237\u540D",dataIndex:"userName",key:"applicationUserName",align:"center",initialValue:""},{title:"\u8BBF\u95EE\u6B21\u6570",dataIndex:"visitedCount",key:"visitedCount",search:!1,width:100,align:"center"},{title:"\u5E38\u7528\u6570\u636E",dataIndex:"usualVisitedData",key:"usualVisitedData",align:"center",search:!1,render:function($,u){var se="-",N=JSON.parse(u.usualVisitedData);if(N.tableNameDesc)se=N.tableNameDesc;else{var ie=N.tableName.split("#");ie.shift(),ie.length>0?se=ie.join("#"):se=N.tableName}return(0,r.jsx)("div",{children:se})}},{title:"\u6700\u540E\u8BBF\u95EE\u65F6\u95F4",dataIndex:"lastVisitedDate",key:"lastVisitedDate",search:!1,align:"center"},{title:"\u64CD\u4F5C",key:"action",align:"center",search:!1,render:function($,u){return[(0,r.jsx)(a.Z,{type:"link",size:"small",onClick:function(){if(S===u.id){U("");return}if(Se({types:[],intervals:[],ranges:[],higherOrLower:{higher:[],lower:[]}}),u.visitedCount!==0){var N=JSON.parse(u.usualVisitedData),ie=N.tableName,we=ee.Z.getRangeStartAndEndTime(7,"YYYY-mm-dd HH:MM:SS"),$e=ee.Z.getRangeTime(7,"YYYY-mm-dd HH:MM:SS");(0,j.Xd)({applicationUserName:u.userName,startTime:we[0],endTime:we[1],tableName:ie}).then(function(Ie){var Be=Ie.data;if(Be&&Be.length>0){var Le=Be.map(function(z,O){return{year:$e[O],value:z}}),xe=(0,k.Z)(le.q);(0,j.FP)({userName:u.userName}).then(function(z){if(z.data&&z.data.length>0){xe.forEach(function(Z){z.data.forEach(function(G){var Ne=JSON.parse(G),Pe=Ne.dbType,We=Ne.dbTypeTimes;Z.type===Pe&&(Z.value=parseInt(We))})});var O=xe.map(function(Z){return Z.type});z.data.forEach(function(Z){var G=JSON.parse(Z),Ne=G.dbType,Pe=G.dbTypeTimes;O.includes(Ne)||xe.push({type:Ne,value:Pe})})}});var Me=[];(0,j.mt)(u.userName).then(function(z){z.data&&(Me=Object.keys(z.data).map(function(O){return{temp:parseInt(O.replace(/[^0-9]/ig,"")),year:O.replace(/[^0-9]/ig,"")+":00",value:z.data[O]}}).sort(function(O,Z){return O.temp-Z.temp})),(0,j.Bx)({applicationUserName:u.userName}).then(function(O){O.data&&(O.data.high.forEach(function(Z){var G=Z.visitedData.split("#");return Z.visitedData=G[G.length-1],Z}),O.data.low.forEach(function(Z){var G=Z.visitedData.split("#");return Z.visitedData=G[G.length-1],Z}),console.log(xe),Se(function(){return{types:xe,intervals:Me,ranges:Le,higherOrLower:{higher:O.data.high,lower:O.data.low}}}),U(u.id))})})}})}else K.ZP.info("".concat(u.userName,"\u7528\u6237\u8BBF\u95EE\u6B21\u6570\u4E3A0\uFF0C\u6682\u65E0\u8BE6\u60C5\u6570\u636E")).then()},children:"\u8BE6\u60C5"},"action")]}}],Ee=function($){(0,j.F$)($).then(function(u){var se=u.data,N=JSON.parse(se);N?(Fe(N.total),N.rows&&Array.isArray(N.rows)?(console.log(N.rows),l(N.rows.map(function(ie){return ie.id=ee.Z.uuid(),ie}))):l([])):l([])})};return(0,y.useEffect)(function(){Ee(h)},[]),(0,r.jsx)("div",{className:q().userAccess,children:(0,r.jsx)(pe.Z,{headerTitle:"\u7528\u6237\u8BBF\u95EE\u884C\u4E3A\u7EDF\u8BA1\u5217\u8868",columns:Re,rowKey:"id",search:{searchText:"\u67E5\u8BE2",resetText:"\u91CD\u7F6E"},onReset:(0,b.Z)(_().mark(function X(){return _().wrap(function(u){for(;;)switch(u.prev=u.next){case 0:Ee(h);case 1:case"end":return u.stop()}},X)})),pagination:{showSizeChanger:!1,total:Ue,pageSize:h.pageSize,current:h.pageNo,onChange:function($,u){T({pageNo:$,pageSize:u}),Ee({username:F,pageNo:$,pageSize:u})}},options:!1,onSubmit:function(){var X=(0,b.Z)(_().mark(function $(u){return _().wrap(function(N){for(;;)switch(N.prev=N.next){case 0:Object.keys(u).length>0?(ye(u),Ee((0,p.Z)({username:u.applicationUserName},h))):Ee(h);case 1:case"end":return N.stop()}},$)}));return function($){return X.apply(this,arguments)}}(),expandable:{expandedRowRender:function($,u){return(0,r.jsx)(ge,{details:Ce,id:u,style:q(),refreshUid:S===""?"close":"open"},u)},expandIcon:function(){return!1},expandedRowKeys:[S]},dataSource:d})})},ne=M},84674:function(de,V,t){"use strict";t.d(V,{q:function(){return p}});var p=[{type:"insert",value:0},{type:"delete",value:0},{type:"update",value:0},{type:"select",value:0}],b=[{type:"\u4E0A\u5348",value:0},{type:"\u4E0B\u5348",value:0},{type:"\u665A\u4E0A",value:0}]},6650:function(de,V,t){"use strict";t.d(V,{F$:function(){return K},Xd:function(){return R},mt:function(){return _},et:function(){return ee},FP:function(){return pe},Bx:function(){return be}});var p=t(11849),b=t(3182),Q=t(94043),a=t.n(Q),Y=t(25377);function K(o,i){return k.apply(this,arguments)}function k(){return k=(0,b.Z)(a().mark(function o(i,g){return a().wrap(function(e){for(;;)switch(e.prev=e.next){case 0:return e.abrupt("return",(0,Y.WY)("/api/skyflying/getCoarseCountsOfUsers",(0,p.Z)({method:"GET",params:i},g||{})));case 1:case"end":return e.stop()}},o)})),k.apply(this,arguments)}function R(o,i){return B.apply(this,arguments)}function B(){return B=(0,b.Z)(a().mark(function o(i,g){return a().wrap(function(e){for(;;)switch(e.prev=e.next){case 0:return e.abrupt("return",(0,Y.WY)("/api/skyflying/getCountsOfUserRecentSevenDays",(0,p.Z)({method:"GET",params:i},g||{})));case 1:case"end":return e.stop()}},o)})),B.apply(this,arguments)}function _(o,i){return y.apply(this,arguments)}function y(){return y=(0,b.Z)(a().mark(function o(i,g){return a().wrap(function(e){for(;;)switch(e.prev=e.next){case 0:return e.abrupt("return",(0,Y.WY)("/api/skyflying/getVisitRate",(0,p.Z)({method:"GET",params:{username:i}},g||{})));case 1:case"end":return e.stop()}},o)})),y.apply(this,arguments)}function Ze(o,i){return q.apply(this,arguments)}function q(){return q=_asyncToGenerator(_regeneratorRuntime.mark(function o(i,g){return _regeneratorRuntime.wrap(function(e){for(;;)switch(e.prev=e.next){case 0:return e.abrupt("return",request("/api/skyflying/getCountsOfAllRecentSevenDays",_objectSpread({method:"GET",params:i},g||{})));case 1:case"end":return e.stop()}},o)})),q.apply(this,arguments)}function ee(o,i){return le.apply(this,arguments)}function le(){return le=(0,b.Z)(a().mark(function o(i,g){return a().wrap(function(e){for(;;)switch(e.prev=e.next){case 0:return e.abrupt("return",(0,Y.WY)("/api/skyflying/getCountsOfUser",(0,p.Z)({method:"GET",params:i},g||{})));case 1:case"end":return e.stop()}},o)})),le.apply(this,arguments)}function pe(o,i){return j.apply(this,arguments)}function j(){return j=(0,b.Z)(a().mark(function o(i,g){return a().wrap(function(e){for(;;)switch(e.prev=e.next){case 0:return e.abrupt("return",(0,Y.WY)("/api/skyflying/getUserOperationTypeCount",(0,p.Z)({method:"GET",params:i},g||{})));case 1:case"end":return e.stop()}},o)})),j.apply(this,arguments)}function be(o,i){return I.apply(this,arguments)}function I(){return I=(0,b.Z)(a().mark(function o(i,g){return a().wrap(function(e){for(;;)switch(e.prev=e.next){case 0:return e.abrupt("return",(0,Y.WY)("/api/skyflying/getUserUsualAndUnusualData",(0,p.Z)({method:"GET",params:i},g||{})));case 1:case"end":return e.stop()}},o)})),I.apply(this,arguments)}function Te(o,i){return L.apply(this,arguments)}function L(){return L=_asyncToGenerator(_regeneratorRuntime.mark(function o(i,g){return _regeneratorRuntime.wrap(function(e){for(;;)switch(e.prev=e.next){case 0:return e.abrupt("return",request("/api/skyflying/getCoarseCountsOfOneUser",_objectSpread({method:"GET",params:i||{}},g||{})));case 1:case"end":return e.stop()}},o)})),L.apply(this,arguments)}},9536:function(de,V,t){"use strict";t.d(V,{Z:function(){return P}});var p=t(96156),b=t(22122),Q=t(90484),a=t(67294),Y=t(94184),K=t.n(Y),k=t(28481),R=t(81253),B=t(28991),_=t(63441),y=t(21770),Ze=t(42550),q=t(98423),ee=["prefixCls","direction","options","disabled","defaultValue","value","onChange","className","motionName"];function le(e){if(typeof e.title!="undefined")return e.title;if((0,Q.Z)(e.label)!=="object"){var n;return(n=e.label)===null||n===void 0?void 0:n.toString()}}function pe(e){return e.map(function(n){if((0,Q.Z)(n)==="object"&&n!==null){var m=le(n);return(0,B.Z)((0,B.Z)({},n),{},{title:m})}return{label:n==null?void 0:n.toString(),title:n==null?void 0:n.toString(),value:n}})}var j=function(n){return{transform:"translateX(".concat(n.offsetLeft,"px)"),width:n.clientWidth}},be=function(n){var m=n.prefixCls,f=n.className,E=n.disabled,x=n.checked,w=n.label,r=n.title,te=n.value,ue=n.onChange,oe=function(ae){E||ue(ae,te)};return a.createElement("label",{className:K()(f,(0,p.Z)({},"".concat(m,"-item-disabled"),E))},a.createElement("input",{className:"".concat(m,"-item-input"),type:"radio",disabled:E,checked:x,onChange:oe}),a.createElement("div",{className:"".concat(m,"-item-label"),title:r},w))},I=a.forwardRef(function(e,n){var m,f,E=e.prefixCls,x=E===void 0?"rc-segmented":E,w=e.direction,r=e.options,te=e.disabled,ue=e.defaultValue,oe=e.value,H=e.onChange,ae=e.className,W=ae===void 0?"":ae,ve=e.motionName,me=ve===void 0?"thumb-motion":ve,fe=(0,R.Z)(e,ee),A=a.useRef(null),ge=(0,Ze.sQ)(A,n),M=a.useRef({from:null,to:null}),ne=a.useMemo(function(){return pe(r)},[r]),ce=(0,y.Z)((m=ne[0])===null||m===void 0?void 0:m.value,{value:e.value,defaultValue:ue}),s=(0,k.Z)(ce,2),c=s[0],d=s[1],l=a.useState(c),C=(0,k.Z)(l,2),v=C[0],S=C[1],U=a.useState(!1),D=(0,k.Z)(U,2),re=D[0],F=D[1],ye=a.useCallback(function(h){var T,J=ne.findIndex(function(Re){return Re.value===h});if(!(J<0)){var he=(T=A.current)===null||T===void 0?void 0:T.querySelector(".".concat(x,"-item:nth-child(").concat(J+1,")"));if(he){var Ce,Se=(Ce=A.current)===null||Ce===void 0?void 0:Ce.querySelector(".".concat(x,"-item-selected"));Se&&he&&M.current&&(M.current.from=j(Se),M.current.to=j(he),F(!0))}}},[x,ne]),De=a.useRef(v);a.useEffect(function(){De.current=v}),a.useEffect(function(){(typeof c=="string"||typeof c=="number")&&c!==De.current&&ye(c)},[c]);var Oe=function(T,J){te||(d(J),H==null||H(J))},Ue=function(){var T=M.current.from;if(T)return S(void 0),T},Fe=function(){var T=M.current.to;if(T)return T},je=function(){F(!1),S(c),M.current&&(M.current={from:null,to:null})},Ae=(0,q.Z)(fe,["children"]);return a.createElement("div",(0,B.Z)((0,B.Z)({},Ae),{},{className:K()(x,(f={},(0,p.Z)(f,"".concat(x,"-rtl"),w==="rtl"),(0,p.Z)(f,"".concat(x,"-disabled"),te),f),W),ref:ge}),a.createElement(_.Z,{visible:re,motionName:"".concat(x,"-").concat(me),motionDeadline:300,onEnterStart:Ue,onEnterActive:Fe,onEnterEnd:je},function(h){var T=h.className,J=h.style;return a.createElement("div",{style:J,className:K()("".concat(x,"-thumb"),T)})}),ne.map(function(h){return a.createElement(be,(0,B.Z)({key:h.value,prefixCls:x,className:K()(h.className,"".concat(x,"-item"),(0,p.Z)({},"".concat(x,"-item-selected"),h.value===v)),checked:h.value===c,onChange:Oe},h))}))});I.displayName="Segmented",I.defaultProps={options:[]};var Te=I,L=t(65632),o=t(97647),i=function(e,n){var m={};for(var f in e)Object.prototype.hasOwnProperty.call(e,f)&&n.indexOf(f)<0&&(m[f]=e[f]);if(e!=null&&typeof Object.getOwnPropertySymbols=="function")for(var E=0,f=Object.getOwnPropertySymbols(e);E<f.length;E++)n.indexOf(f[E])<0&&Object.prototype.propertyIsEnumerable.call(e,f[E])&&(m[f[E]]=e[f[E]]);return m},g=a.forwardRef(function(e,n){var m,f=e.prefixCls,E=e.className,x=e.block,w=e.options,r=e.size,te=r===void 0?"middle":r,ue=i(e,["prefixCls","className","block","options","size"]),oe=a.useContext(L.E_),H=oe.getPrefixCls,ae=oe.direction,W=H("segmented",f),ve=a.useContext(o.Z),me=te||ve,fe=a.useMemo(function(){return w.map(function(A){if((0,Q.Z)(A)==="object"&&(A==null?void 0:A.icon)){var ge=A.icon,M=A.label,ne=i(A,["icon","label"]);return(0,b.Z)((0,b.Z)({},ne),{label:a.createElement(a.Fragment,null,a.createElement("span",{className:"".concat(W,"-item-icon")},ge),a.createElement("span",null,M))})}return A})},[w,W]);return a.createElement(Te,(0,b.Z)({},ue,{className:K()(E,(m={},(0,p.Z)(m,"".concat(W,"-block"),x),(0,p.Z)(m,"".concat(W,"-sm"),me==="small"),(0,p.Z)(m,"".concat(W,"-lg"),me==="large"),m)),options:fe,ref:n,prefixCls:W,direction:ae}))});g.displayName="Segmented",g.defaultProps={options:[]};var P=g},90387:function(de,V,t){"use strict";var p=t(38663),b=t.n(p),Q=t(99210),a=t.n(Q)}}]);
