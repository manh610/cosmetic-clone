package com.cosmetic.gg.service.product.impl;

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
import com.cosmetic.gg.common.enums.ErrorCode;
import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.common.utils.SecurityUserUtils;
import com.cosmetic.gg.common.utils.mapper.ModelMapper;
import com.cosmetic.gg.common.utils.string.StringUtils;
import com.cosmetic.gg.dto.response.attribute.SkinTypeResponse;
import com.cosmetic.gg.dto.response.attribute.ValueDetailResponse;
import com.cosmetic.gg.dto.response.discount.ProductDiscountDetailResponse;
import com.cosmetic.gg.dto.response.product.ProductDetailResponse;
import com.cosmetic.gg.dto.response.product.ProductResponse;
import com.cosmetic.gg.entity.attribute.ProductImage;
import com.cosmetic.gg.entity.attribute.ProductItem;
import com.cosmetic.gg.entity.attribute.ProductSkinType;
import com.cosmetic.gg.entity.attribute.SkinType;
import com.cosmetic.gg.entity.attribute.ValueDetail;
import com.cosmetic.gg.entity.product.Brand;
import com.cosmetic.gg.entity.product.Category;
import com.cosmetic.gg.entity.product.Product;
import com.cosmetic.gg.model.product.ProductModel;
import com.cosmetic.gg.repository.attribute.ProductImageRepository;
import com.cosmetic.gg.repository.attribute.ProductItemRepository;
import com.cosmetic.gg.repository.attribute.ProductSkinTypeRepository;
import com.cosmetic.gg.repository.attribute.SkinTypeRepository;
import com.cosmetic.gg.repository.attribute.ValueDetailRepository;
import com.cosmetic.gg.repository.discount.DiscountRepository;
import com.cosmetic.gg.repository.order.OrderItemRepository;
import com.cosmetic.gg.repository.product.BrandRepository;
import com.cosmetic.gg.repository.product.CategoryRepository;
import com.cosmetic.gg.repository.product.FavoriteRepository;
import com.cosmetic.gg.repository.product.ProductRepository;
import com.cosmetic.gg.repository.product.ReviewRepository;
import com.cosmetic.gg.service.product.ProductService;
@Service
public class ProductServiceImpl implements ProductService{

	private static final Logger log = LoggerFactory.getLogger(ProductServiceImpl.class);
	
	@Autowired
	private ProductRepository productRepository;
	
	@Autowired 
	private CategoryRepository categoryRepository;
	
	@Autowired
	private BrandRepository brandRepository;
	
	@Autowired
	private ProductImageRepository imageProductRepository;
	
	@Autowired
	private SkinTypeRepository skinTypeRepository;
	
	@Autowired
	private ProductSkinTypeRepository productSkinTypeRepository;
	
	@Autowired
	private ProductItemRepository productItemRepository;
	
	@Autowired
	private ValueDetailRepository valueDetailRepository;
	
	@Autowired
	private DiscountRepository discountRepository;
	
	@Autowired
	private FavoriteRepository favoriteRepository;
	
	@Autowired
	private OrderItemRepository orderItemRepository;
	
	@Autowired
	private ReviewRepository reviewRepository;
	
	@Override
	public Map<String, Object> search(String keyword, EStatus status, String brandId, String categoryId, String skinTypeId, 
								Float min, Float max, Boolean isDate, Integer pageIndex, Integer pageSize) {
		Map<String, Object> result = new HashMap<>();
		try {
			pageSize = pageSize == 0 ? CommonConstant.DEFAULT_PAGE_SIZE : pageSize;
			pageIndex = pageIndex > 0 ? (pageIndex - 1)*pageSize : 0;
			min = min == -1.0f ? null : min;
			max = max == -1.0f ? null : max;
			if(min != null && max != null) 
				if(min>max)
					return null;

			List<ProductResponse> products = new ArrayList<>();
			List<Integer> totalItem = new ArrayList<>();
			if(Boolean.TRUE.equals(StringUtils.isNullOrEmpty(skinTypeId))) {
				List<Object> objects = productRepository.search(
						Boolean.TRUE.equals(StringUtils.isNullOrEmpty(keyword)) ? null : keyword,
						Objects.isNull(status) ? null : status.name(),
						Boolean.TRUE.equals(StringUtils.isNullOrEmpty(brandId)) ? null : brandId,
						Boolean.TRUE.equals(StringUtils.isNullOrEmpty(categoryId)) ? null : categoryId,
						min, max, 
						Objects.isNull(isDate) ? null : isDate,
						pageIndex, pageSize);
				for(Object rs: objects) {
					if(rs instanceof Object[]) {
						Object[] objArray = (Object[]) rs;
						ProductResponse productResponse = new ProductResponse();
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
						
						List<String> skinTypePairs = Arrays.asList(((String) objArray[15]).split(","));
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
						Integer totalFavorite = favoriteRepository.cntProductFavorite((String) objArray[0]);
						productResponse.setTotalFavorite(totalFavorite);
						Integer cnt = productRepository.cntTotalSellByProduct(productResponse.getId());
						cnt = cnt == null ? 0 : cnt;
						productResponse.setTotalSell(cnt);
						
						Integer cntStar = reviewRepository.cntStarByProduct(productResponse.getId());
						Integer totalReview = reviewRepository.cntReviewByProduct(productResponse.getId());
						Double totalStar = 0.0;
						if(cntStar != null && totalReview != null)
							totalStar = (cntStar*1.0)/totalReview;
						productResponse.setTotalStar(totalStar);
						productResponse.setTotalReview(totalReview);
						products.add(productResponse);
					}
				}
				
				totalItem = productRepository.cntProduct(
						Boolean.TRUE.equals(StringUtils.isNullOrEmpty(keyword)) ? null : keyword,
						Objects.isNull(status) ? EStatus.STOCK.name() : status.name(),
						Boolean.TRUE.equals(StringUtils.isNullOrEmpty(brandId)) ? null : brandId,
						Boolean.TRUE.equals(StringUtils.isNullOrEmpty(categoryId)) ? null : categoryId,
						min, max,
						Objects.isNull(isDate) ? null : isDate);
			}else {
			
				List<Object> objects = productRepository.searchBySkinType(
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(keyword)) ? null : keyword,
					Objects.isNull(status) ? EStatus.STOCK.name() : status.name(),
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(brandId)) ? null : brandId,
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(categoryId)) ? null : categoryId,
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(skinTypeId)) ? null : skinTypeId,
					min, max, 
					Objects.isNull(isDate) ? null : isDate,
					pageIndex, pageSize);
				for(Object rs: objects) {
					if(rs instanceof Object[]) {
						Object[] objArray = (Object[]) rs;
						ProductResponse productResponse = new ProductResponse();
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
						
						List<String> skinTypePairs = Arrays.asList(((String) objArray[15]).split(","));
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
						Integer totalFavorite = favoriteRepository.cntProductFavorite((String) objArray[0]);
						productResponse.setTotalFavorite(totalFavorite);
						Integer cnt = productRepository.cntTotalSellByProduct(productResponse.getId());
						cnt = cnt == null ? 0 : cnt;
						productResponse.setTotalSell(cnt);
						
						Integer cntStar = reviewRepository.cntStarByProduct(productResponse.getId());
						Integer totalReview = reviewRepository.cntReviewByProduct(productResponse.getId());
						Double totalStar = 0.0;
						if(cntStar != null && totalReview != null)
							totalStar = (cntStar*1.0)/totalReview;
						productResponse.setTotalStar(totalStar);
						productResponse.setTotalReview(totalReview);
						products.add(productResponse);
					}
				}
				
				totalItem = productRepository.cntProductBySkinType(
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(keyword)) ? null : keyword,
					Objects.isNull(status) ? EStatus.STOCK.name() : status.name(),
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(brandId)) ? null : brandId,
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(categoryId)) ? null : categoryId,
					Boolean.TRUE.equals(StringUtils.isNullOrEmpty(skinTypeId)) ? null : skinTypeId,
					min, max,
					Objects.isNull(isDate) ? null : isDate);
			}
			result.put("data", products);
			result.put("totalItem", totalItem.size());
		}catch(Exception ex) {
			result.put("data", new ArrayList<Product>());
			result.put("totalItem", 0);
			log.error("Error while searching product", ex.getCause());
		}
		return result;
	}
	
	public Product getById(String id) {
		try {
			Product productEntity = productRepository.findById(id).orElse(null);
			if(productEntity == null)
				productEntity = null;
			
			return productEntity;
		}catch(Exception ex) {
			log.error(String.format("Error while getting product by id: %s", id), ex.getCause());
			return null;
		}
	}
	
	@Override
	public List<Error> validator(ProductModel productModel) {
		List<Error> errors = new ArrayList<>();
		try {
			Product productCheck;
			if(Boolean.FALSE.equals(StringUtils.isNullOrEmpty(productModel.getId()))) {
				productCheck = productRepository.findById(productModel.getId()).orElse(null);
				if(productCheck == null)
					errors.add(new Error().builder(ErrorCode.NOT_FOUND));
				
				assert productCheck != null;
				if(!productCheck.getCode().equals(productModel.getCode()))
					errors.add(new Error().builder(ErrorCode.INVALID_DATA, "Ký hiệu không được thay đổi", "Code can not be change"));
			}
			
			productCheck = productRepository.findByKey(productModel.getCode());
			if(productCheck != null && !productCheck.getId().equals(productModel.getId()))
				errors.add(new Error().builder(ErrorCode.EXIST_CODE));
			
			Category category = categoryRepository.findById(productModel.getCategoryId()).orElse(null);
			if(category == null || category.getStatus() == EStatus.DELETED)
				errors.add(new Error().builder(ErrorCode.NOT_FOUND, "Danh mục không hợp lệ", "Category is invalid"));
			
			Brand brand = brandRepository.findById(productModel.getBrandId()).orElse(null);
			if(brand == null || brand.getStatus() == EStatus.DELETED)
				errors.add(new Error().builder(ErrorCode.NOT_FOUND, "Thương hiệu/Nhãn hàng không hợp lệ", "Brand is invalid"));
			
			if(productModel.getProductionDate() == null || productModel.getExpirationDate() == null)
				errors.add(new Error().builder(ErrorCode.PRODUCT_INVALID_DATE));
			else if (productModel.getProductionDate().isAfter(productModel.getExpirationDate()) || 
					productModel.getProductionDate().equals(productModel.getExpirationDate()) ||
					productModel.getExpirationDate().isBefore(LocalDateTime.now()))
				errors.add(new Error().builder(ErrorCode.PRODUCT_INVALID_DATE));
			
			List<String> skinTypeIds = productModel.getSkinTypeId();
			SkinType skinTypeEntity;
			if(skinTypeIds.isEmpty())
				errors.add(new Error().builder(ErrorCode.PRODUCT_INVALID_SKIN_TYPE));
			for(String id: skinTypeIds) {
				skinTypeEntity = skinTypeRepository.findById(id).orElse(null);
				if(skinTypeEntity == null || skinTypeEntity.getId() == null)
					errors.add(new Error().builder(ErrorCode.PRODUCT_INVALID_SKIN_TYPE));
			}
			
		}catch(Exception ex) {
			log.error("Error while validating brand data", ex.getCause());
			errors.add(new Error().builder(ErrorCode.EXCEPTION));
		}
		return errors;
	}
	
	@Override
	public Product create(ProductModel productModel) {
		try {
			Product productEntity = ModelMapper.map(productModel, Product.class);
			productEntity.prepareEntity();
			productEntity.setStatus(productModel.getStatus());
			productEntity = productRepository.save(productEntity);
			if (Boolean.TRUE.equals(StringUtils.isNullOrEmpty(productEntity.getId())))
		        return null;
			
			List<String> skinTypeIds = productModel.getSkinTypeId();
			for(String id: skinTypeIds) {
				ProductSkinType productSkinType = new ProductSkinType();
				productSkinType.setId("");
				productSkinType.setProductId(productEntity.getId());
				productSkinType.setSkinTypeId(id);
				productSkinType = productSkinTypeRepository.save(productSkinType);
			}
			
			return productEntity;
		}catch(Exception ex) {
			log.error("Error while creating product", ex.getCause());
		    return null;
		}
	}
	
	@Override
	public Product update(ProductModel productModel) {
		try {
			Product productEntity = ModelMapper.map(productModel, Product.class);
			if(productEntity == null)
				return null;
			productEntity.prepareEntity();
			productEntity.setStatus(productModel.getStatus());
			productEntity = productRepository.save(productEntity);
			if(productEntity == null)
				return null;
			
			List<ProductSkinType> productSkinTypes = productSkinTypeRepository.findByKey(productModel.getId());
			for(ProductSkinType element: productSkinTypes) {
				productSkinTypeRepository.deleteById(element.getId());
			}
			List<String> skinTypeIds = productModel.getSkinTypeId();
			for(String id: skinTypeIds) {
				ProductSkinType productSkinType = new ProductSkinType();
				productSkinType.setId("");
				productSkinType.setProductId(productEntity.getId());
				productSkinType.setSkinTypeId(id);
				productSkinType = productSkinTypeRepository.save(productSkinType);
			}
			return productEntity;
		}catch(Exception ex) {
			log.error("Error while updating product", ex.getCause());
		    return null;
		}
	}
	
	@Override
	public Error delete(String id) {
		try {
			Product productEntity = getById(id);
			if(productEntity == null)
				return new Error().builder(ErrorCode.NOT_FOUND);
			productRepository.deleteById(id);
			
			List<ProductImage> imageProducts = imageProductRepository.findByKey(id);
			for(ProductImage element: imageProducts)
				imageProductRepository.deleteById(element.getId());
			
			List<ProductSkinType> productSkinTypes = productSkinTypeRepository.findByKey(id);
			for(ProductSkinType element: productSkinTypes)
				productSkinTypeRepository.deleteById(element.getId());
			
			
			List<ValueDetail> valueDetails = valueDetailRepository.findByKey(id);
			for(ValueDetail element: valueDetails)
				valueDetailRepository.deleteById(element.getId());
			
			List<ProductItem> productItems = productItemRepository.findByKeys(id);
			for(ProductItem element: productItems)
				productItemRepository.deleteById(element.getId());
			return new Error().builder(ErrorCode.SUCCESS);
		}catch(Exception ex) {
			log.error("Error while deleting brand", ex.getCause());
			return new Error().builder(ErrorCode.EXCEPTION);
		}
	}
	
	@Override
	public ErrorCode changePhoto(MultipartFile photo, String id) {
		try {
			Product productEntity = getById(id);
			if (productEntity == null)
		        return ErrorCode.NOT_FOUND;
			
			if (productEntity.getStatus() == EStatus.DELETED)
		    	return ErrorCode.INVALID_STATUS;
			
			String originalFilename = photo.getOriginalFilename();
	        String extension = originalFilename.substring(originalFilename.lastIndexOf(".") + 1);
	        if (!StringUtils.isNullOrEmpty(originalFilename) && originalFilename.length() > 0) {
	        	if (!extension.equals("png") && !extension.equals("jpg") && !extension.equals("gif") 
	        			&& !extension.equals("svg") && !extension.equals("jpeg"))
	        		return ErrorCode.INVALID_IMAGE;
	        }
	        byte[] img = photo.getBytes();
			if(img == null) {
				log.error("Error while convert image");
				return ErrorCode.ERROR_CONVERT_IMAGE;
			}
			productEntity.setPhoto(img);
			productEntity = productRepository.save(productEntity);
			if (Boolean.TRUE.equals(StringUtils.isNullOrEmpty(productEntity.getId())))
				return ErrorCode.FAILURE;
			return ErrorCode.SUCCESS;
		}catch(Exception ex) {
			log.error("Error while changing photo product", ex.getCause());
			return ErrorCode.EXCEPTION;
		}
	}
	
	@Override
	public ErrorCode deleteMultiImage(List<String> ids) {
		try {
			for(String id: ids) {
				ProductImage productImage = imageProductRepository.findById(id).orElse(null);
				if(productImage == null || productImage.getId() == null) {
					return ErrorCode.NOT_FOUND;
				}
			}
			for(String id: ids)
				imageProductRepository.deleteById(id);
			return ErrorCode.SUCCESS;
		}catch(Exception ex) {
			log.error("Error while deleting image product", ex.getCause());
			return ErrorCode.EXCEPTION;
		}
	}
	
	@Override
	public ErrorCode deleteImage(String id) {
		try {
			ProductImage productImage = imageProductRepository.findById(id).orElse(null);
			if(productImage == null || productImage.getId() == null) {
				return ErrorCode.NOT_FOUND;
			}
			imageProductRepository.deleteById(id);
			return ErrorCode.SUCCESS;
		}catch(Exception ex) {
			log.error("Error while deleting image product", ex.getCause());
			return ErrorCode.EXCEPTION;
		}
	}
	
	@Override
	public ErrorCode changeImage(List<MultipartFile> images, String id) {
		try {
			Product productEntity = getById(id);
			if (productEntity == null)
		        return ErrorCode.NOT_FOUND;
			
			if (productEntity.getStatus() == EStatus.DELETED)
		    	return ErrorCode.INVALID_STATUS;
			
			List<ProductImage> imageProductOrigins = imageProductRepository.findByKey(id);
//			for(ProductImage imageProductOrigin: imageProductOrigins)
//				imageProductRepository.deleteById(imageProductOrigin.getId());
			
			for(MultipartFile image: images) {
				ProductImage imageProduct = new ProductImage();
				String originalFilename = image.getOriginalFilename();
		        String extension = originalFilename.substring(originalFilename.lastIndexOf(".") + 1);
		        if (!StringUtils.isNullOrEmpty(originalFilename) && originalFilename.length() > 0) {
		        	if (!extension.equals("png") && !extension.equals("jpg") && !extension.equals("gif") 
		        			&& !extension.equals("svg") && !extension.equals("jpeg"))
		        		return ErrorCode.INVALID_IMAGE;
		        }
		        imageProduct.setId("");
				imageProduct.setData(image.getBytes());
				imageProduct.setSize(image.getSize());
				imageProduct.setType(extension);
				String username = SecurityUserUtils.getCurrentUser() == null ?
						"system" : SecurityUserUtils.getCurrentUser().getUsername();
				imageProduct.setUploadedBy(username);
				imageProduct.setUploadedAt(LocalDateTime.now());
				imageProduct.setProductId(id);
				imageProduct = imageProductRepository.save(imageProduct);
				if (Boolean.TRUE.equals(StringUtils.isNullOrEmpty(imageProduct.getId())))
					return ErrorCode.FAILURE;
			}
			
			//kiểm tra nếu save error???
			return ErrorCode.SUCCESS;
		}catch(Exception ex) {
			log.error("Error while changing image product", ex.getCause());
			return ErrorCode.EXCEPTION;
		}
	}
	
	@Override
	public ProductDetailResponse detail(String id) {
		try {
			Product productEntity = productRepository.findById(id).orElse(null);
			if(productEntity == null || productEntity.getId() == null)
				return null; 
			
//			List<ProductImage> images = new ArrayList<>();
			List<ProductImage> images = imageProductRepository.findImage(id);
			
			Integer totalFavorite = favoriteRepository.cntProductFavorite(id);
			
			Integer totalSell = productRepository.cntTotalSellByProduct(id);
			totalSell = totalSell == null ? 0 : totalSell;
			
			Integer cntStar = reviewRepository.cntStarByProduct(id);
			Integer totalReview = reviewRepository.cntReviewByProduct(id);
			Double totalStar = 0.0;
			if(cntStar != null && cntStar != null)
				totalStar = (cntStar*1.0)/totalReview;
			
			List<ValueDetailResponse> valueDetails = new ArrayList<>();
			List<Object> objects = valueDetailRepository.valueDetail(id);
			for(Object obj: objects) {
				if(obj instanceof Object[]) {
					Object[] objArray = (Object[]) obj;
					ValueDetailResponse valueResponse = new ValueDetailResponse();
					valueResponse.setId((String) objArray[0]);
					valueResponse.setImportPrice((Float) objArray[1]);
					valueResponse.setSellPrice((Float) objArray[2]);
					valueResponse.setImportQuantity((Integer) objArray[3]);
					valueResponse.setSellQuantity((Integer) objArray[4]);
					
					EStatus status = ((String) objArray[5]).equalsIgnoreCase("STOCK") ? EStatus.STOCK : EStatus.SOLD_OUT;
					valueResponse.setStatus(status);
					valueResponse.setUnit((String) objArray[6]);
					valueResponse.setDescription((String) objArray[7]);
					valueResponse.setImage((byte[]) objArray[8]);
					valueResponse.setProductItemId((String) objArray[9]);
					valueResponse.setValue((String) objArray[10]);
					
//					Integer cnt = orderItemRepository.cntQuantity(valueResponse.getProductItemId());
//					if(cnt!=0 && cnt!=null) totalSell += cnt;
					
					valueDetails.add(valueResponse);
				}
			}
			
			List<ProductDiscountDetailResponse> productItemDiscounts = new ArrayList<>();
			for(ValueDetailResponse e: valueDetails) {
				List<Object> items = discountRepository.getDiscountByProductItem(e.getProductItemId());
				for(Object obj: items) {
					Object[] objArray = (Object[]) obj;
					ProductDiscountDetailResponse res = new ProductDiscountDetailResponse();
					res.setId((String) objArray[0]);
					res.setCode((String) objArray[1]);
					res.setName((String) objArray[2]);
					res.setStartDate(((java.sql.Timestamp) objArray[3]).toLocalDateTime());
					res.setEndDate(((java.sql.Timestamp) objArray[4]).toLocalDateTime());
					res.setValue((Integer) objArray[5]);
					res.setPath((String) objArray[6]);
					
					EDiscountType discountType = ((String) objArray[7]).equalsIgnoreCase("VOUCHER") ? EDiscountType.VOUCHER : EDiscountType.PROMOTION;
					res.setDiscountType(discountType);
					res.setShow((boolean) objArray[8]);
					res.setImage((byte[]) objArray[9]);
					res.setDescription((String) objArray[10]);
					res.setQuantity((Integer) objArray[11]);
					
					productItemDiscounts.add(res);
				}
				e.setProductItemDiscounts(productItemDiscounts);
			}
			
			List<ProductDiscountDetailResponse> productDiscounts = new ArrayList<>();
			List<Object> variant = discountRepository.getDiscountByProduct(productEntity.getId());
			for(Object obj: variant) {
				Object[] objArray = (Object[]) obj;
				ProductDiscountDetailResponse res = new ProductDiscountDetailResponse();
				res.setId((String) objArray[0]);
				res.setCode((String) objArray[1]);
				res.setName((String) objArray[2]);
				res.setStartDate(((java.sql.Timestamp) objArray[3]).toLocalDateTime());
				res.setEndDate(((java.sql.Timestamp) objArray[4]).toLocalDateTime());
				res.setValue((Integer) objArray[5]);
				res.setPath((String) objArray[6]);
				
				EDiscountType discountType = ((String) objArray[7]).equalsIgnoreCase("VOUCHER") ? EDiscountType.VOUCHER : EDiscountType.PROMOTION;
				res.setDiscountType(discountType);
				res.setShow((boolean) objArray[8]);
				res.setImage((byte[]) objArray[9]);
				res.setDescription((String) objArray[10]);
				res.setQuantity((Integer) objArray[11]);
				
				productDiscounts.add(res);
			}
			
			Object rs = productRepository.detail(id);
			if(rs instanceof Object[]) {
				Object[] objArray = (Object[]) rs;
				ProductDetailResponse productResponse = new ProductDetailResponse();
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
				productResponse.setValueDetails(valueDetails);
				productResponse.setProductDiscounts(productDiscounts);
				productResponse.setTotalFavorite(totalFavorite);
				productResponse.setTotalSell(totalSell);
				productResponse.setTotalReview(totalReview);
				productResponse.setTotalStar(totalStar);
				return productResponse;
			}
			return null;
		}catch(Exception ex) {
			log.error("Error while getting detail product", ex.getCause());
		    return null;
		}
	}
	
	@Override
	public ValueDetailResponse getValueByProductItem(String id) {
		try {
			ProductItem productItem = productItemRepository.checkExist(id);
			if(productItem == null || productItem.getId() ==null)
				return null;
			
			Object rs = valueDetailRepository.getvalueDetailByProductItem(id);
			if(rs instanceof Object[]) {
				Object[] objArray = (Object[]) rs;
				ValueDetailResponse valueResponse = new ValueDetailResponse();
				valueResponse.setId((String) objArray[0]);
				valueResponse.setImportPrice((Float) objArray[1]);
				valueResponse.setSellPrice((Float) objArray[2]);
				valueResponse.setImportQuantity((Integer) objArray[3]);
				valueResponse.setSellQuantity((Integer) objArray[4]);
				
				EStatus status = ((String) objArray[5]).equalsIgnoreCase("STOCK") ? EStatus.STOCK : EStatus.SOLD_OUT;
				valueResponse.setStatus(status);
				valueResponse.setUnit((String) objArray[6]);
				valueResponse.setDescription((String) objArray[7]);
				valueResponse.setImage((byte[]) objArray[8]);
				valueResponse.setProductItemId((String) objArray[9]);
				valueResponse.setValue((String) objArray[10]);
				
				return valueResponse;
			}
			return null;
		}catch(Exception ex) {
			log.error("Error while getting value detail by product item", ex.getCause());
		    return null;
		}
	}
	
//	private ProductDetailResponse detailAttribute (String id) {
//		try {
//			Object rs = productRepository.detailAttribute(id);
//			if(rs instanceof Object[]) {
//				Object[] objArray = (Object[]) rs;
//				ProductDetailResponse response = new ProductDetailResponse();
//				response.setId((String) objArray[0]);
//				response.setCode((String) objArray[1]);
//				response.setName((String) objArray[2]);
//				response.setPhoto((byte[]) objArray[3]);
//				response.setMadeIn((String) objArray[4]);
//				
//				EStatus status = ((String) objArray[5]).equalsIgnoreCase("STOCK") ? EStatus.STOCK : EStatus.SOLD_OUT;
//				response.setStatus(status);
//				
//				response.setProductionDate(((java.sql.Timestamp) objArray[6]).toLocalDateTime());
//				response.setExpirationDate(((java.sql.Timestamp) objArray[7]).toLocalDateTime());
//				response.setDescription((String) objArray[8]);
//				
//				String[] brands = ((String) objArray[9]).split(":");
//				BrandResponse brandResponse = new BrandResponse();
//				if(brands.length == 2) {
//					brandResponse.setId(brands[0]);
//					brandResponse.setName(brands[1]);
//				}
//				response.setBrand(brandResponse);
//				
//				String[] categories = ((String) objArray[10]).split(":");
//				CategoryResponse categoryResponse = new CategoryResponse();
//				if(categories.length == 2) {
//					categoryResponse.setId(categories[0]);
//					categoryResponse.setName(categories[1]);
//				}
//				response.setCategory(categoryResponse);
//				
//				List<String> skinTypePairs = Arrays.asList(((String) objArray[11]).split(","));
//				List<SkinTypeResponse> skinTypeResponses = new ArrayList<>();
//				for(String pair: skinTypePairs) {
//					String[] parts = pair.split(":");
//					if(parts.length == 2) {
//						SkinTypeResponse skinTypeResponse = new SkinTypeResponse();
//						skinTypeResponse.setId(parts[0]);
//						skinTypeResponse.setName(parts[1]);
//						skinTypeResponses.add(skinTypeResponse);
//					}
//				}
//				response.setSkinTypes(skinTypeResponses);
//				response.setMinPrice((Float) objArray[12]);
//				response.setMaxPrice((Float) objArray[13]);
//				
//				List<String> attributePairs = Arrays.asList(((String) objArray[14]).split(","));
//				List<AttributeResponse> attributeResponses = new ArrayList<>();
//				for(String pair: attributePairs) {
//					String[] parts = pair.split(":");
//					if(parts.length == 2) {
//						AttributeResponse attributeResponse = new AttributeResponse();
//						attributeResponse.setAttributeId(parts[0]);
//						attributeResponse.setAttributeName(parts[1]);
//						attributeResponses.add(attributeResponse);
//					}
//				}
//				response.setAttributeResponse(attributeResponses);
//				
//				Object rs2 = productRepository.findVariant(id);
//				if(rs2 instanceof Object) {
//					Object objArray2 = (Object) rs2;
//					List<String> valuePairs = Arrays.asList(((String) objArray2).split(","));
//					List<ValueResponse> valueResponses = new ArrayList<>();
//					for(String pair: valuePairs) {
//						String[] parts = pair.split(":");
//						if(parts.length == 3) {
//							ValueResponse valueResponse = new ValueResponse();
//							valueResponse.setOptionValueId(parts[0]);
//							valueResponse.setOptionValueName(parts[1]);
//							valueResponses.add(valueResponse);
//							for(AttributeResponse att: attributeResponses) {
//								if(att.getAttributeId().equalsIgnoreCase(parts[2])) {
//									att.setValueAttribute(valueResponses);
//								}
//							}
//						}
//					}
//				}
//				
//				BigDecimal a= (BigDecimal) objArray[15];
//				response.setTotalQuantity((Integer) a.intValue());
//				
//				List<ProductImage> images = imageProductRepository.findImage(id);
//				List<ImageProductResponse> imageResponses = new ArrayList<>();
//				for(ProductImage image: images) {
//					ImageProductResponse imageResponse = new ImageProductResponse();
//					imageResponse.setData(image.getData());
//					imageResponses.add(imageResponse);
//				}
//				response.setImages(imageResponses);
//				return response;
//			}
//			return null;
//		}catch(Exception ex) {
//			log.error("Error while getting detail product has attribute", ex.getCause());
//		    return null;
//		}
//	}
//	
//	private ProductDetailResponse detailNoAttribute (String id) {
//		try {
//			Object rs = productRepository.detailNoAttribute(id);
//			if(rs instanceof Object[]) {
//				Object[] objArray = (Object[]) rs;
//				ProductDetailResponse response = new ProductDetailResponse();
//				response.setId((String) objArray[0]);
//				response.setCode((String) objArray[1]);
//				response.setName((String) objArray[2]);
//				response.setPhoto((byte[]) objArray[3]);
//				response.setMadeIn((String) objArray[4]);
//				
//				EStatus status = ((String) objArray[5]).equalsIgnoreCase("STOCK") ? EStatus.STOCK : EStatus.SOLD_OUT;
//				response.setStatus(status);
//				
//				response.setProductionDate(((java.sql.Timestamp) objArray[6]).toLocalDateTime());
//				response.setExpirationDate(((java.sql.Timestamp) objArray[7]).toLocalDateTime());
//				response.setDescription((String) objArray[8]);
//				
//				String[] brands = ((String) objArray[9]).split(":");
//				BrandResponse brandResponse = new BrandResponse();
//				if(brands.length == 2) {
//					brandResponse.setId(brands[0]);
//					brandResponse.setName(brands[1]);
//				}
//				response.setBrand(brandResponse);
//				
//				String[] categories = ((String) objArray[10]).split(":");
//				CategoryResponse categoryResponse = new CategoryResponse();
//				if(categories.length == 2) {
//					categoryResponse.setId(categories[0]);
//					categoryResponse.setName(categories[1]);
//				}
//				response.setCategory(categoryResponse);
//				
//				List<String> skinTypePairs = Arrays.asList(((String) objArray[11]).split(","));
//				List<SkinTypeResponse> skinTypeResponses = new ArrayList<>();
//				for(String pair: skinTypePairs) {
//					String[] parts = pair.split(":");
//					if(parts.length == 2) {
//						SkinTypeResponse skinTypeResponse = new SkinTypeResponse();
//						skinTypeResponse.setId(parts[0]);
//						skinTypeResponse.setName(parts[1]);
//						skinTypeResponses.add(skinTypeResponse);
//					}
//				}
//				response.setSkinTypes(skinTypeResponses);
//				response.setMinPrice((Float) objArray[12]);
//				response.setMaxPrice((Float) objArray[13]);
//				
//				BigDecimal a= (BigDecimal) objArray[14];
//				response.setTotalQuantity((Integer) a.intValue());
//				
//				List<ProductImage> images = imageProductRepository.findImage(id);
//				List<ImageProductResponse> imageResponses = new ArrayList<>();
//				for(ProductImage image: images) {
//					ImageProductResponse imageResponse = new ImageProductResponse();
//					imageResponse.setData(image.getData());
//					imageResponses.add(imageResponse);
//				}
//				response.setImages(imageResponses);
//				return response;
//			}
//			return null;
//		}catch(Exception ex) {
//			log.error("Error while getting detail product no attribute", ex.getCause());
//		    return null;
//		}
//	}
}
