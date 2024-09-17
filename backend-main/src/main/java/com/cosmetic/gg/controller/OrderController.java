package com.cosmetic.gg.controller;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import com.cosmetic.gg.common.constants.CommonConstant;
import com.cosmetic.gg.common.dto.Response;
import com.cosmetic.gg.common.enums.EDeliveryType;
import com.cosmetic.gg.common.enums.EPaymentMethod;
import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.common.enums.EUserRank;
import com.cosmetic.gg.common.enums.ErrorCode;
import com.cosmetic.gg.common.exception.GlobalException;
import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.common.utils.datetime.DateUtils;
import com.cosmetic.gg.common.utils.string.StringUtils;
import com.cosmetic.gg.common.validator.ModelValidator;
import com.cosmetic.gg.dto.request.order.OrderItemProductRequest;
import com.cosmetic.gg.dto.request.order.OrderItemRequest;
import com.cosmetic.gg.dto.response.order.orderDetailResponse;
import com.cosmetic.gg.dto.response.product.ProductItemResponse;
import com.cosmetic.gg.entity.attribute.ProductItem;
import com.cosmetic.gg.entity.order.OrderItem;
import com.cosmetic.gg.entity.order.OrderProduct;
import com.cosmetic.gg.model.UserModel;
import com.cosmetic.gg.model.order.OrderModel;
import com.cosmetic.gg.repository.attribute.ProductItemRepository;
import com.cosmetic.gg.repository.order.OrderRepository;
import com.cosmetic.gg.service.order.OrderService;
import com.cosmetic.gg.service.user.UserService;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@RestController
@RequestMapping(path = "/api/v1/order")
@AllArgsConstructor
@CrossOrigin
public class OrderController {

	private final OrderService orderService;
	
	@Autowired
	private OrderRepository orderRepository;
	
	@Autowired
	private ProductItemRepository productItemRepository;
	
	@GetMapping(value = "")
	@ResponseStatus(value = HttpStatus.OK)
	public ResponseEntity<Response<?>> search(
			@RequestParam(value = "userId", required = false, defaultValue = "") String userId,
			@RequestParam(value = "discountId", required = false, defaultValue = "") String discountId,
			@RequestParam(value = "deliveryUnitId", required = false, defaultValue = "") String deliveryUnitId,
			@RequestParam(value = "censor", required = false, defaultValue = "") String censor,
			@RequestParam(value = "shipper", required = false, defaultValue = "") String shipper,
			@RequestParam(value = "deliveryType", required = false, defaultValue = "") EDeliveryType deliveryType,
			@RequestParam(value = "status", required = false, defaultValue = "") String status,
			@RequestParam(value = "isPayment", required = false, defaultValue = "") Boolean isPayment,
			@RequestParam(value = "orderDateFrom", required = false, defaultValue = "") String orderDateFrom,
		    @RequestParam(value = "orderDateTo", defaultValue = "") String orderDateTo,
		    @RequestParam(value = "provinceId", required = false, defaultValue = "") String provinceId,
		    @RequestParam(value = "districtId", required = false, defaultValue = "") String districtId,
		    @RequestParam(value = "wardId", required = false, defaultValue = "") String wardId,
			@RequestParam(value = "pageIndex", required = false, defaultValue = "1") int pageIndex,
			@RequestParam(value = "pageSize", required = false, defaultValue = "10") int pageSize) {
		String transId = UUID.randomUUID().toString();
		try {
			LocalDateTime start = DateUtils.stringToLocalDate(orderDateFrom, DateUtils.YYYYMMDD_T_HHMMSS);
		    LocalDateTime end = DateUtils.stringToLocalDate(orderDateTo, DateUtils.YYYYMMDD_T_HHMMSS);
			Map<String, Object> result = orderService.search(userId, discountId, deliveryUnitId, censor, shipper, deliveryType,
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(status)) ?
					          null : Arrays.stream(Pattern.compile("[-;,|&]").split(status)).map(EStatus::valueOf).collect(Collectors.toList()),
					isPayment, start, end, provinceId, districtId, wardId, pageIndex, pageSize);
			Response<Object> response = new Response<>().success(
					transId,
					Integer.parseInt(result.get("totalItem").toString()),
					result.get("data")
			);
			return new ResponseEntity<>(
					response,
					HttpStatus.OK
			);
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@PostMapping(value = "")
	@ResponseStatus(value = HttpStatus.CREATED)
	public ResponseEntity<Response<?>> create(
			@RequestBody @Valid OrderModel orderModel,
			BindingResult bindingResult,
			HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			orderModel.setId("");
			orderModel = orderService.create(orderModel);
			if (Objects.isNull(orderModel)) {
				ErrorCode errorCode = ErrorCode.FAILURE;
				return new ResponseEntity<>(
						new Response<Error>().error(transId, new Error().builder(errorCode)),
						HttpStatus.BAD_REQUEST
				);
			}
			
			return new ResponseEntity<>(
					new Response<OrderModel>().success(
							transId,
							orderModel
							),
					HttpStatus.CREATED
					);
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@PutMapping(value = "/{id}",
			produces = {MediaType.APPLICATION_JSON_VALUE},
			consumes = {MediaType.APPLICATION_JSON_VALUE})
	@ResponseStatus(value = HttpStatus.OK)
	public ResponseEntity<Response<?>> update(
			@PathVariable(value = "id") String id,
			@RequestBody @Valid OrderModel orderModel,
			BindingResult bindingResult,
			HttpServletRequest httpServletRequest){
		String transId = UUID.randomUUID().toString();
		try {
			orderModel.setId(id);
			List<Error> errors = orderService.validator(orderModel);
			if (!errors.isEmpty())
				return new ResponseEntity<>(
						new Response<Error[]>().error(
								transId,
								errors.toArray(new Error[0])
						),
						HttpStatus.BAD_REQUEST
				);
			
			orderModel = orderService.update(orderModel);
			if (Objects.isNull(orderModel)) {
				ErrorCode errorCode = ErrorCode.FAILURE;
				return new ResponseEntity<>(
						new Response<Error>().error(transId, new Error().builder(errorCode)),
						HttpStatus.BAD_REQUEST
				);
			}
			
			return new ResponseEntity<>(
					new Response<OrderModel>().success(
							transId,
							orderModel
							),
					HttpStatus.OK
					);
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@GetMapping(value = "/{id}")
	@ResponseStatus(value = HttpStatus.OK)
	public ResponseEntity<Response<?>> detail(
			@PathVariable(value = "id") String id,
			HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			orderDetailResponse products = orderService.detail(id);
			return new ResponseEntity<>(
		        new Response<orderDetailResponse>().success(
		          transId,
		          products
		        ),
		        HttpStatus.OK
		      );
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	
	@PostMapping(value = "/item")
	@ResponseStatus(value = HttpStatus.CREATED)
	public ResponseEntity<Response<?>> addProduct(
			@RequestBody @Valid OrderItemRequest orderItemReq,
			BindingResult bindingResult,
			HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			OrderProduct orderCheck = orderRepository.findById(orderItemReq.getOrderId()).orElse(null);
			if(orderCheck == null || orderCheck.getId() == null)
				return new ResponseEntity<>(
						new Response<Error[]>().error(
								transId,
								new Error().builder(ErrorCode.ORDER_INAVLID)
						),
						HttpStatus.BAD_REQUEST
				);
			
			for(OrderItemProductRequest item: orderItemReq.getProductItem()) {
				ProductItem productItemCheck = productItemRepository.checkExist(item.getProductItemId());
				if(productItemCheck == null || productItemCheck.getId() == null)
					return new ResponseEntity<>(
							new Response<Error[]>().error(
									transId,
									new Error().builder(ErrorCode.INVALID_PRODUCT)
							),
							HttpStatus.BAD_REQUEST
					);
			}
			
			for(OrderItemProductRequest item: orderItemReq.getProductItem()) {
				OrderItem orderItem = new OrderItem();
				orderItem.setId("");
				orderItem.setOrderId(orderItemReq.getOrderId());
				orderItem.setProductItemId(item.getProductItemId());
				orderItem.setQuantity(item.getQuantity());
				
				orderItem = orderService.addProduct(orderItem);
				if (Objects.isNull(orderItem)) {
					ErrorCode errorCode = ErrorCode.FAILURE;
					return new ResponseEntity<>(
							new Response<Error>().error(transId, new Error().builder(errorCode)),
							HttpStatus.BAD_REQUEST
					);
				}
			}

			return new ResponseEntity<>(
					new Response<Error>().error(
							transId, 
							new Error().builder(ErrorCode.SUCCESS)),
					HttpStatus.CREATED
			);
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
	@GetMapping(value = "/user/{id}")
	@ResponseStatus(value = HttpStatus.OK)
	public ResponseEntity<Response<?>> getByUser(
			@PathVariable(value = "id") String id,
			HttpServletRequest httpServletRequest) {
		String transId = UUID.randomUUID().toString();
		try {
			List<orderDetailResponse> products = orderService.getOrderByUser(id);
			return new ResponseEntity<>(
		        new Response<List<orderDetailResponse>>().success(
		          transId,
		          products.size(),
		          products
		        ),
		        HttpStatus.OK
		      );
		}catch(Exception ex) {
			log.error(CommonConstant.EXCEPTION, ex.getCause());
			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
		}
	}
	
//	@PutMapping(value = "/item/{id}",
//			produces = {MediaType.APPLICATION_JSON_VALUE},
//			consumes = {MediaType.APPLICATION_JSON_VALUE})
//	@ResponseStatus(value = HttpStatus.OK)
//	public ResponseEntity<Response<?>> updateProduct (
//			@PathVariable(value = "id") String id,
//			@RequestBody @Valid OrderItem orderItem,
//			BindingResult bindingResult,
//			HttpServletRequest httpServletRequest){
//		String transId = UUID.randomUUID().toString();
//		try {
//			orderItem.setId(id);
//			List<Error> errors = orderService.validatorItem(orderItem);
//			if (!errors.isEmpty())
//				return new ResponseEntity<>(
//						new Response<Error[]>().error(
//								transId,
//								errors.toArray(new Error[0])
//						),
//						HttpStatus.BAD_REQUEST
//				);
//			
//			orderItem = orderService.updateProduct(orderItem);
//			if (Objects.isNull(orderItem)) {
//				ErrorCode errorCode = ErrorCode.FAILURE;
//				return new ResponseEntity<>(
//						new Response<Error>().error(transId, new Error().builder(errorCode)),
//						HttpStatus.BAD_REQUEST
//				);
//			}
//			
//			return new ResponseEntity<>(
//					new Response<OrderItem>().success(
//							transId,
//							orderItem
//							),
//					HttpStatus.OK
//					);
//		}catch(Exception ex) {
//			log.error(CommonConstant.EXCEPTION, ex.getCause());
//			throw GlobalException.builder(ErrorCode.EXCEPTION, transId);
//		}
//	}
}
