(self.webpackChunkant_design_pro=self.webpackChunkant_design_pro||[]).push([[628],{26516:function(Q){Q.exports={overview:"overview___ZXPWZ",staticWrap:"staticWrap___2Invp",waringWrap:"waringWrap___AfQ25",bgW:"bgW___3rDWk",center:"center___21dU2",nowrap:"nowrap___spq6f"}},81135:function(Q,L,e){"use strict";e.r(L),e.d(L,{default:function(){return Pe}});var O=e(57663),g=e(71577),z=e(58024),o=e(39144),x=e(13062),P=e(71230),j=e(22385),B=e(61580),w=e(95300),I=e(7277),k=e(89032),l=e(15746),T=e(71748),W=e(33860),C=e(3182),U=e(71153),J=e(60331),S=e(2824),q=e(94043),$=e.n(q),E=e(67294),se=e(26516),G=e.n(se),u=e(13254),i=e(14277),d=e(11849),M=e(46066),a=e(68628),t=e(85893),ye=function(c){var n={dots:!1,innerSlider:!0,slidesToShow:6,slidesToScroll:1,vertical:!0,verticalSwiping:!0,autoplay:!0,speed:2e3,autoplaySpeed:2e3,arrows:!1,cssEase:"linear"};return(0,t.jsxs)("div",{children:[(0,t.jsxs)("div",{children:[(0,t.jsx)(a.Z,{className:"fs-16 ib danger mr-10"}),(0,t.jsx)("h3",{className:"danger fs-16 ib",children:"\u544A\u8B66\u4FE1\u606F"})]}),(0,t.jsx)("div",{className:"of-hidden",children:c.warningList.length>0?(0,t.jsx)(M.Z,(0,d.Z)((0,d.Z)({},n),{},{children:c.warningList.map(function(r){return(0,t.jsx)("div",{className:"mt-5 mb-5 text-c lh-2",children:r.alarmContent},r.id)})})):(0,t.jsx)(i.Z,{description:"\u6682\u65E0\u544A\u8B66\u4FE1\u606F"})})]})},_e=ye,Ce=e(69610),Se=e(54941),Ee=e(43028),Ze=e(6783),ee=e(69886),te=e(41694),De=ee.G2.InteractionAction,Oe=ee.G2.registerInteraction,xe=ee.G2.registerAction,je=function(f){(0,Ee.Z)(n,f);var c=(0,Ze.Z)(n);function n(){return(0,Ce.Z)(this,n),c.apply(this,arguments)}return(0,Se.Z)(n,[{key:"active",value:function(){var s=this,v=this.getView(),p=this.context.event;if(p.data){var h=p.data.items,R=v.geometries.filter(function(F){return F.type==="point"});(0,te.S6)(R,function(F){(0,te.S6)(F.elements,function(K){var H=(0,te.cx)(h,function(oe){return oe.data===K.data})!==-1,X=K.shape.getChildren(),Y=(0,S.Z)(X,2),ae=Y[0],ne=Y[1];H?(ae.animate({r:10,opacity:.2},{duration:1800,easing:"easeLinear",repeat:!0}),ne.animate({r:6,opacity:.4},{duration:800,easing:"easeLinear",repeat:!0})):s.resetElementState(K)})})}}},{key:"reset",value:function(){var s=this,v=this.getView(),p=v.geometries.filter(function(h){return h.type==="point"});(0,te.S6)(p,function(h){(0,te.S6)(h.elements,function(R){s.resetElementState(R)})})}},{key:"resetElementState",value:function(s){var v=s.shape.getChildren(),p=(0,S.Z)(v,2),h=p[0],R=p[1];h.stopAnimate(),R.stopAnimate();var F=h.get("attrs"),K=F.r,H=F.opacity;h.attr({r:K,opacity:H});var X=R.get("attrs"),Y=X.r,ae=X.opacity;R.attr({r:Y,opacity:ae})}},{key:"getView",value:function(){return this.context.view}}]),n}(De),Te=function(c){return(0,E.useEffect)(function(){ee.G2.registerShape("point","custom-point",{draw:function(s,v){var p={x:s.x,y:s.y},h=v.addGroup();return h.addShape("circle",{name:"outer-point",attrs:{x:p.x,y:p.y,fill:s.color||"red",opacity:.5,r:6}}),h.addShape("circle",{name:"inner-point",attrs:{x:p.x,y:p.y,fill:s.color||"red",opacity:1,r:2}}),h}}),xe("custom-marker-action",je),Oe("custom-marker-interaction",{start:[{trigger:"tooltip:show",action:"custom-marker-action:active"}],end:[{trigger:"tooltip:hide",action:"custom-marker-action:reset"}]});var n=new ee.x1("RangeTimeData",{height:138,autoFit:!0,data:c.data,xField:"year",yField:"value",label:{},point:{size:5,shape:"custom-point",style:{fill:"white",stroke:"#5B8FF9",lineWidth:2}},tooltip:{showMarkers:!1,formatter:function(s){return{name:"\u5F53\u65E5\u6536\u96C6\u6570\u91CF",value:s.value+"\u6761"}}},state:{active:{style:{shadowBlur:4,stroke:"#000",fill:"red"}}},interactions:[{type:"custom-marker-interaction"}]});n.render()},[]),(0,t.jsx)("div",{id:"RangeTimeData"})},Ue=Te,ce=e(60355),b=e(24480),be=e(6650),de=e(25377);function Ne(f,c){return ie.apply(this,arguments)}function ie(){return ie=(0,C.Z)($().mark(function f(c,n){return $().wrap(function(s){for(;;)switch(s.prev=s.next){case 0:return s.abrupt("return",(0,de.WY)("/api/skyflying/getCountsOfAllRecentSevenDays",(0,d.Z)({params:c,method:"GET"},n||{})));case 1:case"end":return s.stop()}},f)})),ie.apply(this,arguments)}function Ae(f){return ue.apply(this,arguments)}function ue(){return ue=(0,C.Z)($().mark(function f(c){return $().wrap(function(r){for(;;)switch(r.prev=r.next){case 0:return r.abrupt("return",(0,de.WY)("/api/skyflying/getOverviewOfSystem",(0,d.Z)({method:"GET"},c||{})));case 1:case"end":return r.stop()}},f)})),ue.apply(this,arguments)}var Be=e(66572),we=e(2335),Re=e(52778),Ie=[{title:"\u7528\u6237\u540D",dataIndex:"userName",key:"name",align:"center",render:function(c,n){if(!n.userFrom)return n.userName;var r="",s=JSON.parse(n.userFrom),v=(0,S.Z)(s,1),p=v[0];return p==="scheduleTask"?r="\u5B9A\u65F6\u4EFB\u52A1":r=p,(0,t.jsxs)("div",{children:[(0,t.jsx)("span",{children:n.userName}),(0,t.jsx)(J.Z,{style:{marginLeft:5},color:"#108ee9",children:r})]})}},{title:"\u8BBF\u95EE\u6B21\u6570",dataIndex:"visitedCount",key:"visitedCount",search:!1,align:"center",sorter:function(c,n){return n.visitedCount-c.visitedCount},render:function(c,n){return(0,t.jsx)("div",{children:n.visitedCount.toLocaleString()})}},{title:"\u5E38\u7528\u6570\u636E",dataIndex:"usualVisitedData",key:"usualVisitedData",align:"center",search:!1,render:function(c,n){var r="-",s=JSON.parse(n.usualVisitedData);if(s.tableNameDesc)r=s.tableNameDesc;else{var v=s.tableName.split("#");v.shift(),v.length>0?r=v.join("#"):r=s.tableName}return(0,t.jsx)("div",{children:r})}},{title:"\u6700\u540E\u8BBF\u95EE\u65F6\u95F4",dataIndex:"lastVisitedDate",key:"lastVisitedDate",search:!1,align:"center"}],We=[{title:"\u6570\u636E\u5E93",dataIndex:"dbName",key:"dbName",align:"center",search:!1},{title:"\u6570\u636E\u5E93\u8868",dataIndex:"tableName",key:"tableName",align:"center",render:function(c,n){var r="-";if(n.tableNameDesc)r=n.tableNameDesc;else{var s=n.tableName.split("#");r=s[s.length-1]}return(0,t.jsx)("div",{style:{wordWrap:"break-word",wordBreak:"break-word"},children:r})}},{title:"\u88AB\u8BBF\u95EE\u6B21\u6570",dataIndex:"visitedCount",key:"visitedCount",align:"center",sorter:function(c,n){return n.visitedCount-c.visitedCount},render:function(c,n){return(0,t.jsx)("div",{children:n.visitedCount.toLocaleString()})}},{title:"\u5E38\u8BBF\u95EE\u4EBA\u5458",key:"usualVisitedUser",dataIndex:"usualVisitedUser",align:"center",search:!1,render:function(c,n){if(!n.userFrom)return n.usualVisitedUser;var r=JSON.parse(n.userFrom),s=(0,S.Z)(r,1),v=s[0],p=v==="scheduleTask"?"\u5B9A\u65F6\u4EFB\u52A1":v;return(0,t.jsxs)("div",{children:[(0,t.jsx)("span",{children:n.usualVisitedUser}),n.userFrom&&(0,t.jsx)(J.Z,{style:{marginLeft:5},color:"#108ee9",children:p})]})}},{title:"\u6700\u540E\u8BBF\u95EE\u65F6\u95F4",key:"lastVisitedDate",dataIndex:"lastVisitedDate",valueType:"date",hideInSetting:!0,align:"center"}],Me=function(c){var n=(0,E.useState)(),r=(0,S.Z)(n,2),s=r[0],v=r[1],p=(0,E.useState)([]),h=(0,S.Z)(p,2),R=h[0],F=h[1],K=(0,E.useState)([]),H=(0,S.Z)(K,2),X=H[0],Y=H[1],ae=(0,E.useState)([]),ne=(0,S.Z)(ae,2),oe=ne[0],me=ne[1],ke=(0,E.useState)(),pe=(0,S.Z)(ke,2),$e=pe[0],Ge=pe[1],Le=(0,E.useState)(0),fe=(0,S.Z)(Le,2),Fe=fe[0],Ve=fe[1],Ke=(0,E.useState)({pageSize:10,pageNo:1}),Ye=(0,S.Z)(Ke,1),le=Ye[0],ze=(0,E.useState)({pageSize:10,pageNo:1}),Je=(0,S.Z)(ze,1),re=Je[0],He=(0,E.useState)({dbInstance:0,table:0,user:0,visitedInformation:0}),ve=(0,S.Z)(He,2),N=ve[0],Xe=ve[1];function Qe(){(0,we.cD)({pageSize:100,pageNo:1}).then(function(A){var m=A.data;if(m){var y=JSON.parse(m);if(y){var Z=JSON.parse(y.rows);me(Z.map(function(D,_){return D.matchRuleId=_,D}))}else me([])}})}var ge=function(m){(0,be.F$)(m).then(function(y){var Z=y.data,D=JSON.parse(Z);D&&(Ge(D.total),D.rows&&Array.isArray(D.rows)&&v(D.rows.map(function(_){return _.id=b.Z.uuid(),_}).sort(function(_,V){return V.visitedCount-_.visitedCount})))})},qe=function(){var m=b.Z.getRangeStartAndEndTime(7,"YYYY-mm-dd HH:MM:SS"),y=b.Z.getRangeTime(7,"mm-dd");Ne({startTime:m[0],endTime:m[1]}).then(function(Z){var D=Z.data;F(function(){return D.map(function(_,V){return{year:y[V],value:_}})})})},et=function(){var A=(0,C.Z)($().mark(function m(){var y,Z;return $().wrap(function(_){for(;;)switch(_.prev=_.next){case 0:return _.next=2,Ae();case 2:y=_.sent,Z=y.data,Xe(Z);case 5:case"end":return _.stop()}},m)}));return function(){return A.apply(this,arguments)}}(),tt=function(){var A=(0,C.Z)($().mark(function m(){return $().wrap(function(Z){for(;;)switch(Z.prev=Z.next){case 0:case"end":return Z.stop()}},m)}));return function(){return A.apply(this,arguments)}}(),he=function(m){(0,Be.m)(m).then(function(y){var Z=y.data;if(Z){var D=JSON.parse(Z);if(Ve(D.total),D.rows){var _=JSON.parse(D.rows);Y(_.map(function(V){return V.id=b.Z.uuid(),V}).sort(function(V,at){return at.visitedCount-V.visitedCount}));return}}Y([])})};return(0,E.useEffect)(function(){ge(le),qe(),he(re),et().then(),tt().then(),Qe()},[re]),(0,t.jsxs)("div",{className:G().overview,children:[(0,t.jsx)("div",{className:G().staticWrap,children:(0,t.jsxs)(P.Z,{gutter:20,children:[(0,t.jsx)(l.Z,{span:8,children:(0,t.jsx)(W.Z,{active:!0,loading:R.length===0,children:(0,t.jsx)(Re.Z,{title:"\u8FD1\u4E03\u5929\u6570\u636E\u6536\u96C6\u60C5\u51B5",chart:(0,t.jsx)("div",{style:{height:"163px"},children:(0,t.jsx)(Ue,{data:R})})})})}),(0,t.jsx)(l.Z,{span:8,children:(0,t.jsx)(o.Z,{className:G().bgW,title:(0,t.jsx)("div",{className:"text-c",children:"\u6570\u636E\u7EDF\u8BA1\u60C5\u51B5"}),children:(0,t.jsx)(P.Z,{gutter:20,className:"text-c",children:(0,t.jsxs)(W.Z,{active:!0,loading:R.length===0,children:[(0,t.jsx)(l.Z,{span:12,className:"mt-15 text-c",children:(0,t.jsx)(B.Z,{placement:"top",title:N.visitedInformation>=b.Z.BIG_NUMBER?N.visitedInformation.toLocaleString():"",children:(0,t.jsx)(I.Z,{title:"\u5DF2\u6536\u96C6\u4FE1\u606F",value:b.Z.bigNumberTransform(N.visitedInformation),valueStyle:{color:"#3f8600"}})})}),(0,t.jsx)(l.Z,{span:12,className:"mt-15",children:(0,t.jsx)(B.Z,{placement:"top",title:N.dbInstance>=b.Z.BIG_NUMBER?N.dbInstance.toLocaleString():"",children:(0,t.jsx)(I.Z,{title:"\u6570\u636E\u5E93\u4E2A\u6570",value:b.Z.bigNumberTransform(N.dbInstance),valueStyle:{color:"#3f8600"}})})}),(0,t.jsx)(l.Z,{span:12,className:"mt-20",children:(0,t.jsx)(B.Z,{placement:"top",title:N.user>=b.Z.BIG_NUMBER?N.user.toLocaleString():"",children:(0,t.jsx)(I.Z,{title:"\u7528\u6237\u4EBA\u6570",value:b.Z.bigNumberTransform(N.user),valueStyle:{color:"#3f8600"}})})}),(0,t.jsx)(l.Z,{span:12,className:"mt-20",children:(0,t.jsx)(B.Z,{placement:"top",title:N.table>=b.Z.BIG_NUMBER?N.table.toLocaleString():"",children:(0,t.jsx)(I.Z,{title:"\u6570\u636E\u5E93\u8868",value:b.Z.bigNumberTransform(N.table),valueStyle:{color:"#3f8600"}})})})]})})})}),(0,t.jsx)(l.Z,{span:8,children:(0,t.jsx)("div",{className:G().waringWrap,children:(0,t.jsx)(_e,{warningList:oe})})})]})}),(0,t.jsx)("div",{className:G().center,children:(0,t.jsxs)(P.Z,{gutter:30,children:[(0,t.jsx)(l.Z,{span:12,children:(0,t.jsx)(ce.ZP,{columns:Ie,dataSource:s,scroll:{y:500},options:!1,toolBarRender:function(){return[(0,t.jsx)(g.Z,{type:"link",size:"small",onClick:function(){c.history.push("/user-access")},children:"\u8BE6\u60C5"},"user-link")]},rowKey:"id",search:!1,pagination:{showSizeChanger:!1,total:$e,pageSize:le.pageSize,current:le.pageNo,onChange:function(m,y){ge({pageNo:m,pageSize:y})}},headerTitle:"\u7528\u6237\u8BBF\u95EE\u884C\u4E3A"})}),(0,t.jsx)(l.Z,{span:12,className:"pr-10",children:(0,t.jsx)(W.Z,{active:!0,loading:!1,paragraph:{rows:5},children:(0,t.jsx)(ce.ZP,{scroll:{y:500},columns:We,dataSource:X,options:!1,rowKey:"id",search:!1,pagination:{showSizeChanger:!1,total:Fe,pageSize:re.pageSize,current:re.pageNo,onChange:function(m,y){he({pageNo:m,pageSize:y})}},toolBarRender:function(){return[(0,t.jsx)(g.Z,{type:"link",size:"small",onClick:function(){c.history.push("/databases")},children:"\u8BE6\u60C5"},"data-link")]},headerTitle:"\u6570\u636E\u8BBF\u95EE\u7EDF\u8BA1"})})})]})})]})},Pe=Me},66572:function(Q,L,e){"use strict";e.d(L,{m:function(){return P}});var O=e(11849),g=e(3182),z=e(94043),o=e.n(z),x=e(25377);function P(B,w){return j.apply(this,arguments)}function j(){return j=(0,g.Z)(o().mark(function B(w,I){return o().wrap(function(l){for(;;)switch(l.prev=l.next){case 0:return l.abrupt("return",(0,x.WY)("/api/skyflying/getCoarseCountsOfTableName",(0,O.Z)({method:"GET",params:w},I||{})));case 1:case"end":return l.stop()}},B)})),j.apply(this,arguments)}},6650:function(Q,L,e){"use strict";e.d(L,{F$:function(){return P},Xd:function(){return B},mt:function(){return I},et:function(){return W},FP:function(){return U},Bx:function(){return S},us:function(){return se}});var O=e(11849),g=e(3182),z=e(94043),o=e.n(z),x=e(25377);function P(u,i){return j.apply(this,arguments)}function j(){return j=(0,g.Z)(o().mark(function u(i,d){return o().wrap(function(a){for(;;)switch(a.prev=a.next){case 0:return a.abrupt("return",(0,x.WY)("/api/skyflying/getCoarseCountsOfUsers",(0,O.Z)({method:"GET",params:i},d||{})));case 1:case"end":return a.stop()}},u)})),j.apply(this,arguments)}function B(u,i){return w.apply(this,arguments)}function w(){return w=(0,g.Z)(o().mark(function u(i,d){return o().wrap(function(a){for(;;)switch(a.prev=a.next){case 0:return a.abrupt("return",(0,x.WY)("/api/skyflying/getCountsOfUserRecentSevenDays",(0,O.Z)({method:"GET",params:i},d||{})));case 1:case"end":return a.stop()}},u)})),w.apply(this,arguments)}function I(u,i){return k.apply(this,arguments)}function k(){return k=(0,g.Z)(o().mark(function u(i,d){return o().wrap(function(a){for(;;)switch(a.prev=a.next){case 0:return a.abrupt("return",(0,x.WY)("/api/skyflying/getVisitRate",(0,O.Z)({method:"GET",params:{username:i}},d||{})));case 1:case"end":return a.stop()}},u)})),k.apply(this,arguments)}function l(u,i){return T.apply(this,arguments)}function T(){return T=_asyncToGenerator(_regeneratorRuntime.mark(function u(i,d){return _regeneratorRuntime.wrap(function(a){for(;;)switch(a.prev=a.next){case 0:return a.abrupt("return",request("/api/skyflying/getCountsOfAllRecentSevenDays",_objectSpread({method:"GET",params:i},d||{})));case 1:case"end":return a.stop()}},u)})),T.apply(this,arguments)}function W(u,i){return C.apply(this,arguments)}function C(){return C=(0,g.Z)(o().mark(function u(i,d){return o().wrap(function(a){for(;;)switch(a.prev=a.next){case 0:return a.abrupt("return",(0,x.WY)("/api/skyflying/getCountsOfUser",(0,O.Z)({method:"GET",params:i},d||{})));case 1:case"end":return a.stop()}},u)})),C.apply(this,arguments)}function U(u,i){return J.apply(this,arguments)}function J(){return J=(0,g.Z)(o().mark(function u(i,d){return o().wrap(function(a){for(;;)switch(a.prev=a.next){case 0:return a.abrupt("return",(0,x.WY)("/api/skyflying/getUserOperationTypeCount",(0,O.Z)({method:"GET",params:i},d||{})));case 1:case"end":return a.stop()}},u)})),J.apply(this,arguments)}function S(u,i){return q.apply(this,arguments)}function q(){return q=(0,g.Z)(o().mark(function u(i,d){return o().wrap(function(a){for(;;)switch(a.prev=a.next){case 0:return a.abrupt("return",(0,x.WY)("/api/skyflying/getUserUsualAndUnusualData",(0,O.Z)({method:"GET",params:i},d||{})));case 1:case"end":return a.stop()}},u)})),q.apply(this,arguments)}function $(u,i){return E.apply(this,arguments)}function E(){return E=_asyncToGenerator(_regeneratorRuntime.mark(function u(i,d){return _regeneratorRuntime.wrap(function(a){for(;;)switch(a.prev=a.next){case 0:return a.abrupt("return",request("/api/skyflying/getCoarseCountsOfOneUser",_objectSpread({method:"GET",params:i||{}},d||{})));case 1:case"end":return a.stop()}},u)})),E.apply(this,arguments)}function se(u,i){return G.apply(this,arguments)}function G(){return G=(0,g.Z)(o().mark(function u(i,d){return o().wrap(function(a){for(;;)switch(a.prev=a.next){case 0:return a.abrupt("return",(0,x.WY)("/api/skyflying/getCountsOfEveryonRecentSevenDays",(0,O.Z)({method:"GET",params:i||{}},d||{})));case 1:case"end":return a.stop()}},u)})),G.apply(this,arguments)}},2335:function(Q,L,e){"use strict";e.d(L,{rO:function(){return P},cD:function(){return B},sF:function(){return I}});var O=e(11849),g=e(3182),z=e(94043),o=e.n(z),x=e(25377);function P(l){return j.apply(this,arguments)}function j(){return j=(0,g.Z)(o().mark(function l(T){return o().wrap(function(C){for(;;)switch(C.prev=C.next){case 0:return C.abrupt("return",(0,x.WY)("/api/skyflying/getUserNameAnomalyDetectionInfo",(0,O.Z)({method:"GET"},T||{})));case 1:case"end":return C.stop()}},l)})),j.apply(this,arguments)}function B(l,T){return w.apply(this,arguments)}function w(){return w=(0,g.Z)(o().mark(function l(T,W){return o().wrap(function(U){for(;;)switch(U.prev=U.next){case 0:return U.abrupt("return",(0,x.WY)("/api/skyflying/getAllAlarmInfoDetailByUserName",(0,O.Z)({method:"GET",params:T||{}},W||{})));case 1:case"end":return U.stop()}},l)})),w.apply(this,arguments)}function I(l,T){return k.apply(this,arguments)}function k(){return k=(0,g.Z)(o().mark(function l(T,W){return o().wrap(function(U){for(;;)switch(U.prev=U.next){case 0:return U.abrupt("return",(0,x.WY)("/api/skyflying/getAnomalyDetectionInfoByGroupByUserName",(0,O.Z)({method:"GET",params:T||{}},W||{})));case 1:case"end":return U.stop()}},l)})),k.apply(this,arguments)}}}]);