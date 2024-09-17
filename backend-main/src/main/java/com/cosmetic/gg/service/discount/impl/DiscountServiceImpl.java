package com.cosmetic.gg.service.discount.impl;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.cosmetic.gg.common.constants.CommonConstant;
import com.cosmetic.gg.common.enums.EDiscountType;
import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.common.enums.EUserRank;
import com.cosmetic.gg.common.enums.ErrorCode;
import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.common.utils.mapper.ModelMapper;
import com.cosmetic.gg.common.utils.string.StringUtils;
import com.cosmetic.gg.dto.request.discount.ProductDiscountRequest;
import com.cosmetic.gg.dto.request.discount.ProductItemDiscountRequest;
import com.cosmetic.gg.dto.request.discount.UserDiscountRequest;
import com.cosmetic.gg.dto.response.attribute.SkinTypeResponse;
import com.cosmetic.gg.dto.response.discount.DiscountResponse;
import com.cosmetic.gg.dto.response.discount.ProductDiscountResponse;
import com.cosmetic.gg.dto.response.product.ProductItemResponse;
import com.cosmetic.gg.entity.User;
import com.cosmetic.gg.entity.attribute.ProductImage;
import com.cosmetic.gg.entity.attribute.ProductItem;
import com.cosmetic.gg.entity.discount.Discount;
import com.cosmetic.gg.entity.discount.ProductDiscount;
import com.cosmetic.gg.entity.discount.ProductItemDiscount;
import com.cosmetic.gg.entity.discount.UserDiscount;
import com.cosmetic.gg.entity.product.Cart;
import com.cosmetic.gg.entity.product.Product;
import com.cosmetic.gg.repository.UserRepository;
import com.cosmetic.gg.repository.attribute.ProductImageRepository;
import com.cosmetic.gg.repository.attribute.ProductItemRepository;
import com.cosmetic.gg.repository.discount.DiscountRepository;
import com.cosmetic.gg.repository.discount.ProductDiscountRepository;
import com.cosmetic.gg.repository.discount.ProductItemDiscountRepository;
import com.cosmetic.gg.repository.discount.UserDiscountRepository;
import com.cosmetic.gg.repository.product.ProductRepository;
import com.cosmetic.gg.service.discount.DiscountService;

@Service
public class DiscountServiceImpl implements DiscountService {
	private static final Logger log = LoggerFactory.getLogger(DiscountServiceImpl.class);
	
	@Autowired
	private DiscountRepository discountRepository;
	
	@Autowired
	private UserRepository userRepository;
	
	@Autowired
	private UserDiscountRepository userDiscountRepository;
	
	@Autowired
	private ProductItemRepository productItemRepository;
	
	@Autowired
	private ProductItemDiscountRepository productItemDiscountRepository;
	
	@Autowired
	private ProductDiscountRepository productDiscountRepository;
	
	@Autowired
	private ProductRepository productRepository;
	
	@Autowired
	private ProductImageRepository imageProductRepository;
	
	@Override
	public Map<String, Object> search(String keyword, EDiscountType discountType, 
			Integer pageIndex, Integer pageSize) {
		Map<String, Object> result = new HashMap<>();
		try {
			pageSize = pageSize == 0 ? CommonConstant.DEFAULT_PAGE_SIZE : pageSize;
			pageIndex = pageIndex > 0 ? (pageIndex - 1)*pageSize : 0;
			
			List<Discount> discounts = discountRepository.search(
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(keyword)) ? null : keyword,
					Objects.isNull(discountType) ? null : discountType.name(),
					pageIndex, pageSize);
			Integer totalItem = discountRepository.cntDiscount(
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(keyword)) ? null : keyword,
					Objects.isNull(discountType) ? null : discountType.name());
			
			result.put("data", discounts);
			result.put("totalItem", totalItem);
			return result;
		}catch(Exception ex) {
			result.put("data", new ArrayList<>());
			result.put("totalItem", 0);
			log.error("Error while searching user", ex.getCause());
		}
		return result;
	}
	
	@Override
	public List<Error> validator(Discount discount){
		List<Error> errors = new ArrayList<>();
		try {
			Discount discountCheck;
			if(Boolean.FALSE.equals(StringUtils.isNullOrEmpty(discount.getId()))) {
				discountCheck = discountRepository.findById(discount.getId()).orElse(null);
				if(discountCheck == null)
					errors.add(new Error().builder(ErrorCode.NOT_FOUND));
				
				assert discountCheck != null;
				if(!discountCheck.getCode().equals(discount.getCode()))
					errors.add(new Error().builder(ErrorCode.INVALID_DATA, "Ký hiệu không được thay đổi", "Code can not be change"));
			}
			
			discountCheck = discountRepository.findByKey(discount.getCode());
			if(discountCheck != null && !discountCheck.getId().equals(discount.getId()))
				errors.add(new Error().builder(ErrorCode.EXIST_CODE));
			
			if(discount.getEndDate() == null || discount.getStartDate() == null)
				errors.add(new Error().builder(ErrorCode.PRODUCT_INVALID_DATE));
			else if (discount.getStartDate().isAfter(discount.getEndDate()) ||
					discount.getEndDate().isBefore(LocalDateTime.now()))
				errors.add(new Error().builder(ErrorCode.PRODUCT_INVALID_DATE));
		}catch(Exception ex) {
			log.error("Error while validating brand data", ex.getCause());
			errors.add(new Error().builder(ErrorCode.EXCEPTION));
		}
		return errors;
	}
	
	@Override
	public Discount create(Discount discount) {
		try {
			discount = discountRepository.save(discount);
			if (Boolean.TRUE.equals(StringUtils.isNullOrEmpty(discount.getId())))
		        return null;
			
			return discount;
		}catch(Exception ex) {
			log.error("Error while creating discount", ex.getCause());
		    return null;
		}
	}
	
	@Override
	public Discount update(Discount discount) {
		try {
			if(discount == null)
				return null;
			discount = discountRepository.save(discount);
			if(discount == null)
				return null;

			return discount;
		}catch(Exception ex) {
			log.error("Error while updating discount", ex.getCause());
		    return null;
		}
	}
	
	@Override
	public Error delete(String id) {
		try {
//			Discount discountEntity = discountRepository.findById(id).orElse(null);
//			if(discountEntity == null || discountEntity.getId() == null)
//				return new Error().builder(ErrorCode.NOT_FOUND);
//			
//			Integer numberUse = discountRepository.cntUse(id);
//			Integer numberUser = discountRepository.cntUser(id);
//			
//			if(numberUse < numberUser) return new Error().builder(ErrorCode.DISCOUNT_INVALID_DELETE);
//			discountRepository.deleteById(id);
			
			return new Error().builder(ErrorCode.SUCCESS);
		}catch(Exception ex) {
			log.error("Error while deleting discount", ex.getCause());
			return new Error().builder(ErrorCode.EXCEPTION);
		}
	}
	
	@Override
	public ErrorCode changeImage(MultipartFile image, String id) {
		try {
			Discount discountEntity = discountRepository.findById(id).orElse(null);
			if (discountEntity == null || discountEntity.getId() == null)
		        return ErrorCode.NOT_FOUND;
			
			String originalFilename = image.getOriginalFilename();
	        String extension = originalFilename.substring(originalFilename.lastIndexOf(".") + 1);
	        if (!StringUtils.isNullOrEmpty(originalFilename) && originalFilename.length() > 0) {
	        	if (!extension.equals("png") && !extension.equals("jpg") && !extension.equals("gif") 
	        			&& !extension.equals("svg") && !extension.equals("jpeg"))
	        		return ErrorCode.INVALID_IMAGE;
	        }
	        byte[] img = image.getBytes();
			if(img == null) {
				log.error("Error while convert image");
				return ErrorCode.ERROR_CONVERT_IMAGE;
			}
			discountEntity.setImage(img);
			discountEntity = discountRepository.save(discountEntity);
			if (Boolean.TRUE.equals(StringUtils.isNullOrEmpty(discountEntity.getId())))
				return ErrorCode.FAILURE;
			return ErrorCode.SUCCESS;
		}catch(Exception ex) {
			log.error("Error while changing photo discount", ex.getCause());
			return ErrorCode.EXCEPTION;
		}
	}
	
	@Override
	public Error addUserDiscount(UserDiscountRequest discountRequest) {
		try {
			Discount discountCheck = discountRepository.findById(discountRequest.getDiscountId()).orElse(null);
			if(discountCheck == null || discountCheck.getId() == null)
				return new Error().builder(ErrorCode.NOT_FOUND);
			if(discountCheck.getDiscountType() == EDiscountType.PROMOTION) {
				return new Error().builder(ErrorCode.DISCOUNT_INVALID);
			}
			for(String id: discountRequest.getUserIds()) {
				User userEntity = userRepository.findById(id).orElse(null);
				if(userEntity == null || userEntity.getId() == null || userEntity.getStatus() != EStatus.ACTIVE)
					return new Error().builder(ErrorCode.NOT_FOUND);
			}
			for(String id: discountRequest.getUserIds()) {
				UserDiscount userDiscount = new UserDiscount();
				userDiscount.setDiscountId(discountCheck.getId());
				userDiscount.setUserId(id);
				userDiscount.setUse(false);
				
				userDiscount = userDiscountRepository.save(userDiscount);
			}
			return new Error().builder(ErrorCode.SUCCESS);
		}catch(Exception ex) {
			log.error("Error while add discount for user", ex.getCause());
			return new Error().builder(ErrorCode.EXCEPTION);
		}
	}
	
//	@Override
//	public Error deleteAllUserDiscount(String id) {
//		try {
//			Discount discountEntity = discountRepository.findById(id).orElse(null);
//			if(discountEntity == null || discountEntity.getId() == null)
//				return new Error().builder(ErrorCode.NOT_FOUND);
//			
//			List<UserDiscount> userDiscounts = userDiscountRepository.findAllUSerByDiscount(id);
//			for(UserDiscount item: userDiscounts) {
//				userDiscountRepository.deleteById(item.getId());
//			}
//			return new Error().builder(ErrorCode.SUCCESS);
//		}catch(Exception ex) {
//			log.error("Error while deleting all user in discount", ex.getCause());
//			return new Error().builder(ErrorCode.EXCEPTION);
//		}
//	}
	
	public Error addProductDiscount(ProductDiscountRequest productDiscountRequest) {
		try {
			Discount discountEntity = discountRepository.findById(productDiscountRequest.getDiscountId()).orElse(null);
			if(discountEntity == null || discountEntity.getId() == null)
				return new Error().builder(ErrorCode.NOT_FOUND);
			if(discountEntity.getDiscountType() == EDiscountType.PROMOTION) {
				return new Error().builder(ErrorCode.DISCOUNT_INVALID);
			}
			
			for(String id: productDiscountRequest.getProductIds()) {
				Product productEntity = productRepository.findById(id).orElse(null);
				if(productEntity == null || productEntity.getId() == null)
					return new Error().builder(ErrorCode.NOT_FOUND);
				if(productEntity.getStatus() == EStatus.SOLD_OUT) 
					return new Error().builder(ErrorCode.INVALID_PRODUCT);
			}
			for(String id: productDiscountRequest.getProductIds()) {
				ProductDiscount productDiscount = new ProductDiscount();
				productDiscount.setDiscountId(discountEntity.getId());	
				productDiscount.setQuantity(productDiscountRequest.getQuantity());
				productDiscount.setProductId(id);
				
				productDiscount = productDiscountRepository.save(productDiscount);
			}
			return new Error().builder(ErrorCode.SUCCESS);
		}catch(Exception ex) {
			log.error("Error while adding discount for product", ex.getCause());
			return new Error().builder(ErrorCode.EXCEPTION);
		}
	}
	
	@Override
	public Error addProductItemDiscount(ProductItemDiscountRequest productItemDiscountRequest) {
		try {
			Discount discountEntity = discountRepository.findById(productItemDiscountRequest.getDiscountId()).orElse(null);
			if(discountEntity == null || discountEntity.getId() == null)
				return new Error().builder(ErrorCode.NOT_FOUND);
			if(discountEntity.getDiscountType() == EDiscountType.PROMOTION) {
				return new Error().builder(ErrorCode.DISCOUNT_INVALID);
			}
			
			for(String id: productItemDiscountRequest.getProductItemIds()) {
				ProductItem productItemEntity = productItemRepository.findById(id).orElse(null);
				if(productItemEntity == null || productItemEntity.getId() == null)
					return new Error().builder(ErrorCode.NOT_FOUND);
			}
			for(String id: productItemDiscountRequest.getProductItemIds()) {
				ProductItemDiscount productDiscount = new ProductItemDiscount();
				productDiscount.setDiscountId(discountEntity.getId());	
				productDiscount.setQuantity(productItemDiscountRequest.getQuantity());
				productDiscount.setProductItemId(id);
				
				productDiscount = productItemDiscountRepository.save(productDiscount);
			}
			
			return new Error().builder(ErrorCode.SUCCESS);
		}catch(Exception ex) {
			log.error("Error while adding discount for product item", ex.getCause());
			return new Error().builder(ErrorCode.EXCEPTION);
		}
	}
	
//	@Override
//	public Error deleteAllProductDiscount(String id) {
//		try {
//			Discount discountEntity = discountRepository.findById(id).orElse(null);
//			if(discountEntity == null || discountEntity.getId() == null)
//				return new Error().builder(ErrorCode.NOT_FOUND);
//			
//			List<ProductDiscount> productDiscounts = productDiscountRepository.findAllProductByDiscount(id);
//			for(ProductDiscount item: productDiscounts) {
//				productDiscountRepository.deleteById(item.getId());
//			}
//			return new Error().builder(ErrorCode.SUCCESS);
//		}catch(Exception ex) {
//			log.error("Error while deleting all user in discount", ex.getCause());
//			return new Error().builder(ErrorCode.EXCEPTION);
//		}
//	}

	@Override
	public DiscountResponse detail(String id) {
		try {
			DiscountResponse result = new DiscountResponse();
			Discount discountEntity = discountRepository.findById(id).orElse(null);
			if(discountEntity == null || discountEntity.getId() == null)
				return null;
			List<User> users = getUserByDiscount(id);
			Integer totalUse = discountRepository.cntUse(id);
			
			List<ProductItemResponse> productItems = getProductItemByDiscount(id);
			
			List<ProductDiscountResponse> products = getProductDiscount(id);
			
			result.setId(discountEntity.getId());
			result.setCode(discountEntity.getCode());
			result.setName(discountEntity.getName());
			result.setStartDate(discountEntity.getStartDate());
			result.setEndDate(discountEntity.getEndDate());
			result.setValue(discountEntity.getValue());
			result.setPath(discountEntity.getPath());
			result.setDiscountType(discountEntity.getDiscountType());
			result.setShow(discountEntity.isShow());
			result.setImage(discountEntity.getImage());
			result.setDescription(discountEntity.getDescription());
			result.setTotalUse(totalUse);
			result.setUsers(users);
			result.setProductItems(productItems);
			result.setProducts(products);
			
			return result;
			
		}catch(Exception ex) {
			log.error("Error while getting detail discount", ex.getCause());
			return null;
		}
	}
	
	private List<User> getUserByDiscount(String id) {
		try {
			List<User> users = userRepository.getUserByDiscount(id);
			if(users.isEmpty()) return null;
			return users;
		}catch(Exception ex) {
			log.error("Error while getting user by discount", ex.getCause());
			return null;
		}
	}
	
	private List<ProductDiscountResponse> getProductDiscount(String id) {
		List<ProductDiscountResponse> result = new ArrayList<>();
		try {
			List<Object> objs = productRepository.getProductByDiscount(id);
			for(Object rs: objs) {
				if(rs instanceof Object[]) {
					Object[] objArray = (Object[]) rs;
					ProductDiscountResponse productResponse = new ProductDiscountResponse();
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
					productResponse.setQuantityDiscount((Integer) objArray[11]);
					
					result.add(productResponse);
				}
			}
			return result;
		}catch(Exception ex) {
			log.error("Error while getting product by discount", ex.getCause());
			return null;
		}
	}
	
	private List<ProductItemResponse> getProductItemByDiscount(String id) {
		List<ProductItemResponse> result = new ArrayList<>();
		try {			
			List<ProductItemDiscount> productDiscount = productItemDiscountRepository.getProductItemByDiscount(id);
			if(productDiscount.isEmpty()) 
				return null;
			
			for(ProductItemDiscount item: productDiscount) {
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
					
					List<byte[]> images = new ArrayList<>();
					List<ProductImage> productImage = imageProductRepository.findByProductItem(item.getProductItemId());
					for(ProductImage element: productImage) {
						images.add(element.getData());
					}
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
					productResponse.setQuantityDiscount(item.getQuantity());
					
					result.add(productResponse);
				}
			}
			return result;
		}catch(Exception ex) {
			log.error("Error while getting product by discount", ex.getCause());
		    return null;
		}
	}
}
