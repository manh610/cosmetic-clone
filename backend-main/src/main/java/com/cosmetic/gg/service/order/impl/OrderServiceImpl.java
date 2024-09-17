package com.cosmetic.gg.service.order.impl;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.cosmetic.gg.common.constants.CommonConstant;
import com.cosmetic.gg.common.enums.EDeliveryType;
import com.cosmetic.gg.common.enums.EPaymentMethod;
import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.common.enums.ErrorCode;
import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.common.utils.mapper.ModelMapper;
import com.cosmetic.gg.common.utils.string.StringUtils;
import com.cosmetic.gg.dto.response.attribute.SkinTypeResponse;
import com.cosmetic.gg.dto.response.order.orderDetailResponse;
import com.cosmetic.gg.dto.response.order.orderProductItemResponse;
import com.cosmetic.gg.dto.response.product.ProductItemResponse;
import com.cosmetic.gg.entity.User;
import com.cosmetic.gg.entity.attribute.ProductImage;
import com.cosmetic.gg.entity.attribute.ProductItem;
import com.cosmetic.gg.entity.attribute.ValueDetail;
import com.cosmetic.gg.entity.discount.Discount;
import com.cosmetic.gg.entity.discount.ProductDiscount;
import com.cosmetic.gg.entity.discount.UserDiscount;
import com.cosmetic.gg.entity.order.OrderItem;
import com.cosmetic.gg.entity.order.OrderProduct;
import com.cosmetic.gg.entity.product.Cart;
import com.cosmetic.gg.model.order.OrderModel;
import com.cosmetic.gg.repository.UserRepository;
import com.cosmetic.gg.repository.attribute.ProductImageRepository;
import com.cosmetic.gg.repository.attribute.ProductItemRepository;
import com.cosmetic.gg.repository.attribute.ValueDetailRepository;
import com.cosmetic.gg.repository.discount.DiscountRepository;
import com.cosmetic.gg.repository.discount.ProductDiscountRepository;
import com.cosmetic.gg.repository.discount.UserDiscountRepository;
import com.cosmetic.gg.repository.order.OrderItemRepository;
import com.cosmetic.gg.repository.order.OrderRepository;
import com.cosmetic.gg.repository.product.ProductRepository;
import com.cosmetic.gg.service.order.OrderService;

@Service
public class OrderServiceImpl implements OrderService{

	private static final Logger log = LoggerFactory.getLogger(OrderServiceImpl.class);
	
	@Autowired
	private OrderRepository orderRepository;
	
	@Autowired
	private DiscountRepository discountRepository;
	
	@Autowired
	private OrderItemRepository orderItemRepository;
	
	@Autowired
	private ProductItemRepository productItemRepository;
	
	@Autowired
	private ProductImageRepository imageProductRepository;
	
	@Autowired
	private ProductRepository productRepository;
	
	@Autowired
	private UserRepository userRepository;
	
	@Autowired
	private ValueDetailRepository valueDetailRepository;
	
	@Autowired
	private UserDiscountRepository userDiscountRepository;
	
	@Autowired
	private ProductDiscountRepository productDiscountRepository;
	
	@Override
	public Map<String, Object> search(String userId, String discountId, String deliveryUnitId, String censor, String shipper,
							EDeliveryType deliveryType, List<EStatus> status, Boolean isPayment, LocalDateTime orderDateFrom, 
							LocalDateTime orderDateTo, String provinceId, String districtId, String wardId, 
							Integer pageIndex, Integer pageSize) {
		Map<String, Object> result = new HashMap<>();
		try {
			pageSize = pageSize == 0 ? CommonConstant.DEFAULT_PAGE_SIZE : pageSize;
			pageIndex = pageIndex > 0 ? (pageIndex - 1)*pageSize : 0;
			if(Objects.isNull(orderDateFrom) && !Objects.isNull(orderDateTo))
				orderDateFrom = orderDateTo;
			if(!Objects.isNull(orderDateFrom) && Objects.isNull(orderDateTo))
				orderDateTo = orderDateFrom;
			List<OrderProduct> orders = orderRepository.search(
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(userId)) ? null : userId,
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(discountId)) ? null : discountId,
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(deliveryUnitId)) ? null : deliveryUnitId,
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(censor)) ? null : censor,
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(shipper)) ? null : shipper,
					Objects.isNull(deliveryType) ? null : deliveryType.name(),
					Objects.isNull(status) || status.isEmpty() ?
						Collections.emptyList() : status.stream().map(Enum::name).collect(Collectors.toList()),
					isPayment==null ? null : isPayment,
					Objects.isNull(orderDateFrom) ? null : orderDateFrom,
					Objects.isNull(orderDateTo) ? null : orderDateTo,
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(provinceId)) ? null : provinceId,
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(districtId)) ? null : districtId,
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(wardId)) ? null : wardId,
					pageIndex, pageSize);
			
			Integer totalItem = orderRepository.cntOrder(
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(userId)) ? null : userId,
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(discountId)) ? null : discountId,
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(deliveryUnitId)) ? null : deliveryUnitId,
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(censor)) ? null : censor,
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(shipper)) ? null : shipper,
					Objects.isNull(deliveryType) ? null : deliveryType.name(),
					Objects.isNull(status) || status.isEmpty() ?
						Collections.emptyList() : status.stream().map(Enum::name).collect(Collectors.toList()),
					isPayment==null ? null : isPayment,
					Objects.isNull(orderDateFrom) ? null : orderDateFrom,
					Objects.isNull(orderDateTo) ? null : orderDateTo,
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(provinceId)) ? null : provinceId,
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(districtId)) ? null : districtId,
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(wardId)) ? null : wardId);
			
			result.put("data", orders);
			result.put("totalItem", totalItem);
		}catch (Exception ex) {
	      result.put("data", new ArrayList<>());
	      result.put("totalItem", 0);
	      log.error("Error while searching order product", ex.getCause());
	    }
	    return result;
	}
	
	@Override
	public List<Error> validator(OrderModel orderModel) {
		List<Error> errors = new ArrayList<>();
		try {
			OrderProduct orderCheck;
			if(Boolean.FALSE.equals(StringUtils.isNullOrEmpty(orderModel.getId()))) {
				orderCheck = orderRepository.findById(orderModel.getId()).orElse(null);
				if(orderCheck == null)
					errors.add(new Error().builder(ErrorCode.NOT_FOUND));
				
				assert orderCheck != null;
				if(orderCheck.getStatus() == EStatus.DELIVERED ||
						orderCheck.getStatus() == EStatus.CANCELLED)
					errors.add(new Error().builder(ErrorCode.INVALID_STATUS));
			}
		}catch(Exception ex) {
			log.error("Error while validating order product data", ex.getCause());
			errors.add(new Error().builder(ErrorCode.EXCEPTION));
		}
		return errors;
	}
	
	@Override
	public OrderModel create(OrderModel orderModel) {
		try {
			OrderProduct orderEntity = ModelMapper.map(orderModel, OrderProduct.class);
			if(orderEntity == null || orderEntity.getId() == null)
				return null;
			orderEntity.setOrderDate(LocalDateTime.now());
			if(StringUtils.isNullOrEmpty(orderModel.getPaymentId()) || orderModel.getDeliveryType() == EDeliveryType.GTN) {
				orderEntity.setPayment(false);
				orderEntity.setStatus(EStatus.WAIT_CONFIRM);
			}else {
				orderEntity.setPayment(true);
				orderEntity.setStatus(EStatus.PREPARE);
			}
			Discount discount = discountRepository.checkDiscountByUser(orderEntity.getDiscountId(), orderEntity.getUserId());
			if(discount == null || discount.getId() == null)
				return null;
			
			List<orderProductItemResponse> rs = orderModel.getProductItem();
			for(orderProductItemResponse item : rs) {
				ProductItem productItemCheck = productItemRepository.checkExist(item.getProductItemId());
				if(productItemCheck == null || productItemCheck.getId() == null)
					return null;
			}
			
			OrderModel result = ModelMapper.map(orderRepository.save(orderEntity), OrderModel.class);
			if (result == null)
				return null;
			
			for(orderProductItemResponse item : rs) {
				OrderItem orderItemEntity = new OrderItem();
				orderItemEntity.setOrderId(result.getId());
				orderItemEntity.setProductItemId(item.getProductItemId());
				orderItemEntity.setQuantity(item.getQuantity());
				orderItemEntity = orderItemRepository.save(orderItemEntity);
				
				ValueDetail valueDetailCheck = valueDetailRepository.getvalueDetailByProductItem2(item.getProductItemId());
				if(valueDetailCheck != null && valueDetailCheck.getId() != null) {
					valueDetailCheck.setSellQuantity(valueDetailCheck.getSellQuantity() - 1);
					valueDetailCheck = valueDetailRepository.save(valueDetailCheck);
				}
				ProductDiscount productDiscountCheck = productDiscountRepository.findByDiscountAndProduct(item.getProductItemId(), result.getDiscountId());
				if(productDiscountCheck != null && productDiscountCheck.getId() != null) {
					productDiscountCheck.setQuantity(productDiscountCheck.getQuantity() - 1);
					productDiscountCheck = productDiscountRepository.save(productDiscountCheck);
				}
			}
			UserDiscount userDiscountCheck = userDiscountRepository.findByDiscountAndUser(result.getUserId(), result.getDiscountId());
			if(userDiscountCheck != null && userDiscountCheck.getId() != null) {
				userDiscountCheck.setUse(true);
				userDiscountCheck = userDiscountRepository.save(userDiscountCheck);
			}
			return result;
		}catch(Exception ex) {
			log.error("Error while creating order product", ex.getCause());
			return null;
		}
	}
	
	@Override
	public OrderModel update(OrderModel orderModel) {
		try {
			OrderProduct orderEntity = ModelMapper.map(orderModel, OrderProduct.class);
			if(orderEntity == null || orderEntity.getId() == null)
				return null;
			OrderProduct orderCheck = orderRepository.findById(orderEntity.getId()).orElse(null);
			orderEntity.setOrderDate(orderCheck.getOrderDate());
			OrderModel result = ModelMapper.map(orderRepository.save(orderEntity), OrderModel.class);
			if (result == null)
				return null;
			return result;
		}catch(Exception ex) {
			log.error("Error while updating order product", ex.getCause());
			return null;
		}
	}
	
	@Override
	public orderDetailResponse detail(String id) {
		try {
			OrderProduct orderProduct = orderRepository.findById(id).orElse(null);
			if(orderProduct == null || orderProduct.getId() == null)
				return null;
			List<ProductItemResponse> productItems = new ArrayList<>();
			List<OrderItem> orderItems = orderItemRepository.findByOrder(id);
			for(OrderItem item: orderItems) {
				List<byte[]> images = new ArrayList<>();
				List<ProductImage> productImage = imageProductRepository.findByProductItem(item.getProductItemId());
				for(ProductImage element: productImage) {
					images.add(element.getData());
				}
				
				Object rs = productRepository.getProductByProductItem(item.getProductItemId());
				if(rs instanceof Object[]) {
					Object[] objArray = (Object[]) rs;
					ProductItemResponse productResponse = new ProductItemResponse();
					productResponse.setId((String) objArray[0]);
					productResponse.setName((String) objArray[2]);
					productResponse.setCode((String) objArray[1]);
					productResponse.setPhoto((byte[]) objArray[3]);
					productResponse.setMadeIn((String) objArray[4]);
					
					EStatus statuss = null;
					if(((String) objArray[5]).equalsIgnoreCase("STOCK")) statuss = EStatus.STOCK;
					else if (((String) objArray[5]).equalsIgnoreCase("SOLD_OUT")) statuss = EStatus.SOLD_OUT;
					else statuss = EStatus.HIDDEN;
					productResponse.setStatus(statuss);
					productResponse.setDescription((String) objArray[6]);
					productResponse.setProductionDate(((java.sql.Timestamp) objArray[7]).toLocalDateTime());
					productResponse.setExpirationDate(((java.sql.Timestamp) objArray[8]).toLocalDateTime());
					productResponse.setCategoryId((String) objArray[9]);
					productResponse.setBrandId((String) objArray[10]);
					productResponse.setCategoryName((String) objArray[11]);
					productResponse.setBrandName((String) objArray[12]);
					productResponse.setMinPrice((Float) objArray[13]);
					productResponse.setMaxPrice((Float) objArray[14]);
					
					BigDecimal total= (BigDecimal) objArray[15];
					productResponse.setTotalQuantity((Integer) total.intValue());
					
					List<String> skinTypePairs = Arrays.asList(((String) objArray[16]).split(","));
					List<SkinTypeResponse> skinTypeResponses = new ArrayList<>();
					for(String pair: skinTypePairs) {
						String[] parts = pair.split(":");
						if(parts.length == 2) {
							SkinTypeResponse skinTypeResponse = new SkinTypeResponse();
							skinTypeResponse.setId(parts[0]);
							skinTypeResponse.setName(parts[1]);
							skinTypeResponses.add(skinTypeResponse);
						}
					}
					productResponse.setSkinTypes(skinTypeResponses);
					productResponse.setImages(images);
					productResponse.setValueDetailId((String) objArray[17]);
					productResponse.setProductItemId((String) objArray[18]);
					productResponse.setValue((String) objArray[19]);
					productResponse.setImportPrice((Float) objArray[20]);
					productResponse.setSellPrice((Float) objArray[21]);
					productResponse.setImportQuantity((Integer) objArray[22]);
					productResponse.setSellQuantity((Integer) objArray[23]);
					
					EStatus status = ((String) objArray[24]).equalsIgnoreCase("STOCK") ? EStatus.STOCK : EStatus.SOLD_OUT;
					productResponse.setValueStatus(status);
					productResponse.setUnit((String) objArray[25]);
					productResponse.setImage((byte[]) objArray[26]);
					productResponse.setQuantity(item.getQuantity());
					
					productItems.add(productResponse);
				}
			}
			
			Object rs = orderRepository.detail(id);
			if(rs instanceof Object[]) {
				Object[] objArray = (Object[]) rs;
				orderDetailResponse response = new orderDetailResponse();
				response.setId((String) objArray[0]);
				response.setAddressId((String) objArray[1]);
				
				EDeliveryType type = null;
				if(((String) objArray[2]).equalsIgnoreCase("GHTK")) type = EDeliveryType.GHTK;
				else if (((String) objArray[2]).equalsIgnoreCase("GHN")) type = EDeliveryType.GHN;
				else if (((String) objArray[2]).equalsIgnoreCase("POST")) type = EDeliveryType.POST;
				else type = EDeliveryType.GTN;
				response.setDeliveryType(type);
				
				response.setPaymentId((String) objArray[3]);
				response.setOrderDate(((java.sql.Timestamp) objArray[4]).toLocalDateTime());
				
				LocalDateTime deliveryDate =  Boolean.TRUE.equals(StringUtils.isNullOrEmpty(((String) objArray[5]))) ? null : ((java.sql.Timestamp) objArray[5]).toLocalDateTime();
				response.setDeliveryDate(deliveryDate);
				LocalDateTime receiptDate =  Boolean.TRUE.equals(StringUtils.isNullOrEmpty(((String) objArray[6]))) ? null : ((java.sql.Timestamp) objArray[6]).toLocalDateTime();
				response.setReceiptDate(receiptDate);
				response.setCensorId((String) objArray[7]);
				response.setShipperId((String) objArray[8]);
				response.setPayment((boolean) objArray[9]);
				response.setNote((String) objArray[10]);
				
				EStatus status = null;
				if(((String) objArray[11]).equalsIgnoreCase("WAIT_CONFIRM")) status = EStatus.WAIT_CONFIRM;
				else if (((String) objArray[11]).equalsIgnoreCase("PREPARE")) status = EStatus.PREPARE;
				else if (((String) objArray[11]).equalsIgnoreCase("DELIVERING")) status = EStatus.DELIVERING;
				else if (((String) objArray[11]).equalsIgnoreCase("DELIVERED")) status = EStatus.DELIVERED;
				else if (((String) objArray[11]).equalsIgnoreCase("CANCELLED")) status = EStatus.CANCELLED;
				else status = EStatus.RETURN;
				response.setStatus(status);
				
				response.setTotalPrice((Float) objArray[12]);
				response.setDeliveryUnitId((String) objArray[13]);
				response.setUserId((String) objArray[14]);
				response.setDiscountId((String) objArray[15]);
				response.setProvinceName((String) objArray[16]);
				response.setDistrictName((String) objArray[17]);
				response.setWardName((String) objArray[18]);
				response.setAddressDetail((String) objArray[19]);
				response.setProductItem(productItems);
				
				return response;
			}
			return null;
		}catch(Exception ex) {
			log.error("Error while getting detail order product", ex.getCause());
			return null;
		}
	}
	
	@Override
	public List<Error> validatorItem(OrderItem orderItem){
		List<Error> errors = new ArrayList<>();
		try {
			OrderItem orderItemCheck;
			if(Boolean.FALSE.equals(StringUtils.isNullOrEmpty(orderItem.getId()))) {
				orderItemCheck = orderItemRepository.findById(orderItem.getId()).orElse(null);
				if(orderItemCheck == null)
					errors.add(new Error().builder(ErrorCode.NOT_FOUND));
			}
			ProductItem productItemCheck = productItemRepository.checkExist(orderItem.getProductItemId());
			if(productItemCheck == null || productItemCheck.getId() == null)
				errors.add(new Error().builder(ErrorCode.INVALID_PRODUCT));
			
			OrderProduct orderCheck = orderRepository.findById(orderItem.getOrderId()).orElse(null);
			if(orderCheck == null || orderCheck.getId() == null)
				errors.add(new Error().builder(ErrorCode.ORDER_INAVLID));
		}catch(Exception ex) {
			log.error("Error while validating cart data", ex.getCause());
			errors.add(new Error().builder(ErrorCode.EXCEPTION));
		}
		return errors;
	}
	
	@Override
	public OrderItem addProduct(OrderItem orderItem) {
		try {
			orderItem = orderItemRepository.save(orderItem);
			if (Boolean.TRUE.equals(StringUtils.isNullOrEmpty(orderItem.getId())))
		        return null;
			
			return orderItem;
		}catch(Exception ex) {
			log.error("Error while adding product to order", ex.getCause());
		    return null;
		}
	}
	
	@Override
	public OrderItem updateProduct(OrderItem orderItem) {
		try {
			if(orderItem == null)
				return null;
			orderItem = orderItemRepository.save(orderItem);
			if(orderItem == null)
				return null;
			return orderItem;
		}catch(Exception ex) {
			log.error("Error while updating product in order", ex.getCause());
			return null;
		}
	}
	
	public List<orderDetailResponse> getOrderByUser(String id) {
		List<orderDetailResponse> result = new ArrayList<>();
		try {
			User userCheck = userRepository.findById(id).orElse(null);
			if(userCheck == null || userCheck.getId() == null || userCheck.getStatus() != EStatus.ACTIVE)
				return result;
			
			List<OrderProduct> orderUsers = orderRepository.getOrderByUser(userCheck.getId());
			for(OrderProduct orderUser: orderUsers) {
				List<ProductItemResponse> productItems = new ArrayList<>();
				List<OrderItem> orderItems = orderItemRepository.findByOrder(orderUser.getId());
				for(OrderItem item: orderItems) {
					List<byte[]> images = new ArrayList<>();
					List<ProductImage> productImage = imageProductRepository.findByProductItem(item.getProductItemId());
					for(ProductImage element: productImage) {
						images.add(element.getData());
					}
					
					Object rs = productRepository.getProductByProductItem(item.getProductItemId());
					if(rs instanceof Object[]) {
						Object[] objArray = (Object[]) rs;
						ProductItemResponse productResponse = new ProductItemResponse();
						productResponse.setId((String) objArray[0]);
						productResponse.setName((String) objArray[2]);
						productResponse.setCode((String) objArray[1]);
						productResponse.setPhoto((byte[]) objArray[3]);
						productResponse.setMadeIn((String) objArray[4]);
						
						EStatus statuss = null;
						if(((String) objArray[5]).equalsIgnoreCase("STOCK")) statuss = EStatus.STOCK;
						else if (((String) objArray[5]).equalsIgnoreCase("SOLD_OUT")) statuss = EStatus.SOLD_OUT;
						else statuss = EStatus.HIDDEN;
						productResponse.setStatus(statuss);
						productResponse.setDescription((String) objArray[6]);
						productResponse.setProductionDate(((java.sql.Timestamp) objArray[7]).toLocalDateTime());
						productResponse.setExpirationDate(((java.sql.Timestamp) objArray[8]).toLocalDateTime());
						productResponse.setCategoryId((String) objArray[9]);
						productResponse.setBrandId((String) objArray[10]);
						productResponse.setCategoryName((String) objArray[11]);
						productResponse.setBrandName((String) objArray[12]);
						productResponse.setMinPrice((Float) objArray[13]);
						productResponse.setMaxPrice((Float) objArray[14]);
						
						BigDecimal total= (BigDecimal) objArray[15];
						productResponse.setTotalQuantity((Integer) total.intValue());
						
						List<String> skinTypePairs = Arrays.asList(((String) objArray[16]).split(","));
						List<SkinTypeResponse> skinTypeResponses = new ArrayList<>();
						for(String pair: skinTypePairs) {
							String[] parts = pair.split(":");
							if(parts.length == 2) {
								SkinTypeResponse skinTypeResponse = new SkinTypeResponse();
								skinTypeResponse.setId(parts[0]);
								skinTypeResponse.setName(parts[1]);
								skinTypeResponses.add(skinTypeResponse);
							}
						}
						productResponse.setSkinTypes(skinTypeResponses);
						productResponse.setImages(images);
						productResponse.setValueDetailId((String) objArray[17]);
						productResponse.setProductItemId((String) objArray[18]);
						productResponse.setValue((String) objArray[19]);
						productResponse.setImportPrice((Float) objArray[20]);
						productResponse.setSellPrice((Float) objArray[21]);
						productResponse.setImportQuantity((Integer) objArray[22]);
						productResponse.setSellQuantity((Integer) objArray[23]);
						
						EStatus status = ((String) objArray[24]).equalsIgnoreCase("STOCK") ? EStatus.STOCK : EStatus.SOLD_OUT;
						productResponse.setValueStatus(status);
						productResponse.setUnit((String) objArray[25]);
						productResponse.setImage((byte[]) objArray[26]);
						productResponse.setQuantity(item.getQuantity());
						
						productItems.add(productResponse);
					}
				}
				
				Object rs = orderRepository.detail(orderUser.getId());
				if(rs instanceof Object[]) {
					Object[] objArray = (Object[]) rs;
					orderDetailResponse response = new orderDetailResponse();
					response.setId((String) objArray[0]);
					response.setAddressId((String) objArray[1]);
					
					EDeliveryType type = null;
					if(((String) objArray[2]).equalsIgnoreCase("GHTK")) type = EDeliveryType.GHTK;
					else if (((String) objArray[2]).equalsIgnoreCase("GHN")) type = EDeliveryType.GHN;
					else if (((String) objArray[2]).equalsIgnoreCase("POST")) type = EDeliveryType.POST;
					else type = EDeliveryType.GTN;
					response.setDeliveryType(type);
					
					response.setPaymentId((String) objArray[3]);
					response.setOrderDate(((java.sql.Timestamp) objArray[4]).toLocalDateTime());
					
					LocalDateTime deliveryDate =  Boolean.TRUE.equals(StringUtils.isNullOrEmpty(((String) objArray[5]))) ? null : ((java.sql.Timestamp) objArray[5]).toLocalDateTime();
					response.setDeliveryDate(deliveryDate);
					LocalDateTime receiptDate =  Boolean.TRUE.equals(StringUtils.isNullOrEmpty(((String) objArray[6]))) ? null : ((java.sql.Timestamp) objArray[6]).toLocalDateTime();
					response.setReceiptDate(receiptDate);
					response.setCensorId((String) objArray[7]);
					response.setShipperId((String) objArray[8]);
					response.setPayment((boolean) objArray[9]);
					response.setNote((String) objArray[10]);
					
					EStatus status = null;
					if(((String) objArray[11]).equalsIgnoreCase("WAIT_CONFIRM")) status = EStatus.WAIT_CONFIRM;
					else if (((String) objArray[11]).equalsIgnoreCase("PREPARE")) status = EStatus.PREPARE;
					else if (((String) objArray[11]).equalsIgnoreCase("DELIVERING")) status = EStatus.DELIVERING;
					else if (((String) objArray[11]).equalsIgnoreCase("DELIVERED")) status = EStatus.DELIVERED;
					else if (((String) objArray[11]).equalsIgnoreCase("CANCELLED")) status = EStatus.CANCELLED;
					else status = EStatus.RETURN;
					response.setStatus(status);
					
					response.setTotalPrice((Float) objArray[12]);
					response.setDeliveryUnitId((String) objArray[13]);
					response.setUserId((String) objArray[14]);
					response.setDiscountId((String) objArray[15]);
					response.setProvinceName((String) objArray[16]);
					response.setDistrictName((String) objArray[17]);
					response.setWardName((String) objArray[18]);
					response.setAddressDetail((String) objArray[19]);
					response.setProductItem(productItems);
					
					result.add(response);
				}
			}
			
			return result;
		}catch(Exception ex) {
			log.error("Error while getting order by user", ex.getCause());
			return result;
		}
	}
}
